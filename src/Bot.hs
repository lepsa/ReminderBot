{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot where

import           Bot.Commands
import           Bot.Commands.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Data.Time
import           Data.Traversable
import           Data.Types.App                     (CanAppM, runAppM)
import           Data.Types.DB.Permissions          (checkPermissionIO,
                                                     setPermissionIO)
import           Data.Types.DB.Reminder
import           Data.Types.DB.Schema
import           Data.Types.Env
import           Data.Types.Error
import           Discord                            (DiscordHandler,
                                                     RestCallErrorCode (RestCallErrorCode),
                                                     RunDiscordOpts (discordOnEnd, discordOnEvent, discordOnLog, discordToken),
                                                     def, discordOnStart,
                                                     restCall, runDiscord)
import           Discord.Interactions
import qualified Discord.Internal.Rest.Channel      as R
import qualified Discord.Internal.Rest.Interactions as RI
import qualified Discord.Requests                   as R
import           Discord.Types

type DiscordM c = ReaderT c DiscordHandler
runDiscordM :: c -> DiscordM c a -> DiscordHandler a
runDiscordM c m = runReaderT m c
type CanDiscord m c = (MonadReader c m)

-- | Replies "pong" to every message that starts with "ping"
reminderBot :: CanAppM m c e => m ()
reminderBot = do
  setupDatabase
  createSchema
  runMigrations
  cId <- asks botToken
  c <- ask
  void $ liftIO $ forkIO $ manageThreads c
  userFacingError <- liftIO $ runDiscord $ def
    { discordToken = "Bot " <> cId
    , discordOnEvent = eventHandler c
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    , discordOnStart = do
      sendReminders c
      liftIO $ atomically $ writeTChan (threads c) InitialiseReminder
    , discordOnEnd = do
      atomically $ writeTChan (threads c) StopAll
    } -- if you see OnLog error, post in the discord / open an issue

  liftIO $ TIO.putStrLn userFacingError
  -- userFacingError is an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd (see examples)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent

registerCommands :: DiscordM c ()
registerCommands = pure ()

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show


eventHandler :: HasEnv c => c -> Event -> DiscordHandler ()
eventHandler c event = case event of
  MessageCreate m -> when (isPing m && not (fromBot m)) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    liftIO $ threadDelay (2 * 1_000_000)
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  -- Only sent on initial startup, set up commands and the like here
  Ready _apiVersion _user guilds _sessionId _resumeGatewayUrl _shard (PartialApplication appId _) ->
    onReady appId $ idOnceAvailable <$> guilds
  InteractionCreate i -> onInteractionCreate i
  _ -> return ()
  where
    commands :: [SlashCommand]
    commands = ($ c) <$> slashCommands

    onReady :: ApplicationId -> [GuildId] -> DiscordHandler ()
    onReady appId guilds = do
      echo "Bot ready!"
      guildCmdRegistrations <- for guilds $ \guild ->
        (guild, ) <$> traverse (tryRegistering guild) commands
      for_ guildCmdRegistrations $ \(guild, appCmdRegistrations) ->
        case sequence appCmdRegistrations of
          Left err ->
            echo $ "[!] Failed to register some commands" <> showT err
          Right cmds -> do
            echo $ "Registered " <> showT (length cmds) <> " command(s)."
            unregisterOutdatedCmds guild cmds
      where
      tryRegistering guild cmd = case registration cmd of
        Just reg -> restCall $ R.CreateGuildApplicationCommand appId guild reg
        Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

      unregisterOutdatedCmds guild validCmds = do
        registered <- restCall $ R.GetGuildApplicationCommands appId guild
        case registered of
          Left err -> echo $ "Failed to get registered slash commands: " <> showT err
          Right cmds ->
            let validIds    = applicationCommandId <$> validCmds
                outdatedIds = filter (`notElem` validIds)
                            $ applicationCommandId <$> cmds
             in for_ outdatedIds $ restCall . R.DeleteGuildApplicationCommand appId guild

    onInteractionCreate :: Interaction -> DiscordHandler ()
    onInteractionCreate cmd = case cmd of
      InteractionPing iId _aid tok _v _perms -> void $ restCall $
        RI.CreateInteractionResponse iId tok InteractionResponsePong
      InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
        case find (\sc -> applicationCommandDataName input == name sc) commands of
          Just found -> handler found cmd
          Nothing -> echo "Somehow got unknown slash command (registrations out of date?)"
      _ -> pure () -- Unexpected/unsupported interaction type

manageThreads :: forall c. HasEnv c => c -> IO ()
manageThreads c = do
  reminderThreads <- newTVarIO mempty
  loop reminderThreads
  where
    forkReminder :: TVar (Map InMemoryKey ThreadId) -> Reminder -> IO ()
    forkReminder reminderThreads r = do
      let uuid = reminderId r
      now <- getCurrentTime
      lastRun <- getLastSentIO (conn c) uuid
      threadId <- forkIO $ startReminder c now r lastRun
      atomically $ modifyTVar reminderThreads $ M.insert (reminderName r, reminderGuild r, uuid) threadId

    createReminderChan reminderThreads guild (Register {..}) = do
      either print (forkReminder reminderThreads) <=<
        runAppM @IO @c @AppError c $ createReminder
          registerReminderName
          (toSeconds registerReminderTimeBetween)
          registerReminderMessage
          registerReminderChannel
          guild
      loop reminderThreads

    deleteReminderChan reminderThreads guild ref = do
      m <- readTVarIO reminderThreads
      let mKey = find (\(n', g', u') -> g' == guild && either (== n') (== u') ref) $ M.keys m
      case mKey of
        Nothing -> loop reminderThreads
        Just k -> do
          atomically $ modifyTVar reminderThreads $ M.delete k
          let mThread = M.lookup k m
          traverse_ killThread mThread
          either print pure <=< runAppM @IO @c @AppError c $ deleteReminderByRef guild ref
          loop reminderThreads

    loop reminderThreads = do
      action <- atomically $ readTChan (threads c)
      case action of
        CreateReminderChan guild register channelId userRoles -> do
          allowed <- checkPermissionIO (conn c) guild userRoles
          when allowed $ createReminderChan reminderThreads guild register
          unless allowed $ atomically $ writeTChan (reminderChan c) (channelId, "Invalid Permissions")
        DeleteReminderChan guild register channelId  userRoles -> do
          allowed <- checkPermissionIO (conn c) guild userRoles
          when allowed $ deleteReminderChan reminderThreads guild register
          unless allowed $ atomically $ writeTChan (reminderChan c) (channelId, "Invalid Permissions")
        SetPermissionChan  guild role channelId userRoles -> do
          allowed <- checkPermissionIO (conn c) guild userRoles
          when allowed $ setPermissionIO (conn c) guild role
          unless allowed $ atomically $ writeTChan (reminderChan c) (channelId, "Invalid Permissions")
        InitialiseReminder -> do
          e <- runAppM @IO @c @AppError c getAllReminders
          either print (traverse_ (forkReminder reminderThreads)) e
          loop reminderThreads
        StopAll -> readTVarIO reminderThreads >>= traverse_ killThread

startReminder :: HasEnv c => c -> UTCTime -> Reminder -> Maybe UTCTime -> IO ()
startReminder c currentTime r lastRun =  do
  let secondsBetween = unSeconds $ reminderTimeBetween r
      lastRun' = fromMaybe (reminderCreated r) lastRun
      t' = diffUTCTime currentTime lastRun'
  if t' > fromInteger secondsBetween
  then pure ()
  else delay $ 1_000_000 * truncate t'
  loop secondsBetween
  where
    loop s = do
      atomically $ writeTChan (reminderChan c) (reminderChannel r, reminderMessage r)
      now <- liftIO getCurrentTime
      setLastSentIO (conn c) (reminderId r) now
      delay $ 1_000_000 * s
      loop s

sendReminders :: HasEnv c => c -> DiscordHandler ()
sendReminders c = do
  h <- ask
  void $ liftIO $ forkIO $ runReaderT loop h
  where
    loop :: DiscordHandler ()
    loop = do
      (channel, message) <- liftIO $  atomically $ readTChan $ reminderChan c
      either (liftIO . print) (\_ -> pure ()) <=< restCall $ R.CreateMessage channel message
      loop
