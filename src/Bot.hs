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
import           Data.Types.DB.Permissions          (setPermissionIO)
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
type CanDiscord m c = (MonadReader c m)

runDiscordM :: c -> DiscordM c a -> DiscordHandler a
runDiscordM c m = runReaderT m c

-- | Replies "pong" to every message that starts with "ping"
reminderBot :: CanAppM m c e => m ()
reminderBot = do
  setupDatabase
  createSchema
  runMigrations
  cId <- asks botToken
  c <- ask
  -- Setup reminder threads, and listen for management messages
  void $ liftIO $ forkIO $ manageThreads c
  userFacingError <- liftIO $ runDiscord $ def
    { discordToken = "Bot " <> cId
    , discordOnEvent = eventHandler c
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    , discordOnStart = do
      sendReminders c
      -- Send the message to start the reminders.
      liftIO $ atomically $ writeTChan (threads c) InitialiseReminder
    , discordOnEnd = do
      atomically $ writeTChan (threads c) StopAll
    } -- if you see OnLog error, post in the discord / open an issue

  liftIO $ TIO.putStrLn userFacingError
  -- userFacingError is an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd (see examples)

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show

eventHandler :: HasEnv c => c -> Event -> DiscordHandler ()
eventHandler c event = case event of
  -- Only sent on initial startup, set up commands and the like here
  Ready _apiVersion _user guilds _sessionId _resumeGatewayUrl _shard (PartialApplication aId _) -> do
    -- Set out application ID into config so everything can read it.
    liftIO $ atomically $ writeTVar (appId c) $ Just aId
    echo "Bot ready!"
    onReady aId $ idOnceAvailable <$> guilds
  InteractionCreate i -> onInteractionCreate i
  GuildCreate guild _guildCreateData -> do
    m <- liftIO . readTVarIO $ appId c
    for_ m $ \aId -> onReady aId [guildId guild]
  GuildDelete _guild -> do
    -- TODO delete old reminders, clean up threads
    pure ()
  ChannelDelete _channel -> do
    -- TODO delete old reminders, clean up threads
    pure ()
  _ -> return ()
  where
    commands :: [SlashCommand]
    commands = ($ c) <$> slashCommands
    -- Setup the commands
    onReady :: ApplicationId -> [GuildId] -> DiscordHandler ()
    onReady aId guilds = do
      guildCmdRegistrations <- for guilds $ \guild ->
        (guild, ) <$> traverse (tryRegistering guild) commands
      for_ guildCmdRegistrations $ \(guild, appCmdRegistrations) ->
        case sequence appCmdRegistrations of
          Left err -> echo $ "[!] Failed to register some commands" <> showT err
          Right cmds -> unregisterOutdatedCmds guild cmds
      where
      tryRegistering guild cmd = case registration cmd of
        Just reg -> restCall $ R.CreateGuildApplicationCommand aId guild reg
        Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

      unregisterOutdatedCmds guild validCmds = do
        registered <- restCall $ R.GetGuildApplicationCommands aId guild
        case registered of
          Left err -> echo $ "Failed to get registered slash commands: " <> showT err
          Right cmds -> do
            let validIds    = applicationCommandId <$> validCmds
                outdatedIds = filter (`notElem` validIds) $ applicationCommandId <$> cmds
            for_ outdatedIds $ restCall . R.DeleteGuildApplicationCommand aId guild
    -- Handle interaction events.
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

    deleteReminderChan reminderThreads guild ref = do
      m <- readTVarIO reminderThreads
      let mKey = find (\(n', g', u') -> g' == guild && either (== n') (== u') ref) $ M.keys m
      case mKey of
        Nothing -> pure ()
        Just k -> do
          atomically $ modifyTVar reminderThreads $ M.delete k
          let mThread = M.lookup k m
          traverse_ killThread mThread
          either print pure <=< runAppM @IO @c @AppError c $ deleteReminderByRef guild ref

    loop reminderThreads = do
      action <- atomically $ readTChan (threads c)
      case action of
        CreateReminderChan guild channelId register -> do
          createReminderChan reminderThreads guild register
          atomically $ writeTChan (reminderChan c) (channelId, "Reminder created")
          loop reminderThreads
        DeleteReminderChan guild channelId ref -> do
          deleteReminderChan reminderThreads guild ref
          atomically $ writeTChan (reminderChan c) (channelId, "Reminder deleted")
          loop reminderThreads
        SetPermissionChan guild channelId role -> do
          setPermissionIO (conn c) guild role
          atomically $ writeTChan (reminderChan c) (channelId, "Role set")
          loop reminderThreads
        InitialiseReminder -> do
          e <- runAppM @IO @c @AppError c getAllReminders
          either print (traverse_ (forkReminder reminderThreads)) e
          loop reminderThreads
        StopAll -> readTVarIO reminderThreads >>= traverse_ killThread

startReminder :: HasEnv c => c -> UTCTime -> Reminder -> Maybe UTCTime -> IO ()
startReminder c currentTime r lastRun =  do
  let secondsBetween = unSeconds $ reminderTimeBetween r
      createdOrRan = fromMaybe (reminderCreated r) lastRun
      -- We count when the reminder was created as a "run"
      -- as people don't need a message for the thing they
      -- just set up.
      -- Don't allow this to be negative.
      timeSinceLastReminder = truncate . abs $ diffUTCTime currentTime createdOrRan
  if timeSinceLastReminder > fromInteger secondsBetween
  -- Immediately send a message
  then pure ()
  -- Wait the difference and then send the message
  else delay $ 1_000_000 * (secondsBetween - timeSinceLastReminder)
  loop secondsBetween
  where
    loop s = do
      atomically $ writeTChan (reminderChan c) (reminderChannel r, reminderMessage r)
      now <- liftIO getCurrentTime
      setLastSentIO (conn c) (reminderId r) now
      delay $ 1_000_000 * s
      loop s

-- Listen on the channel to get messages to send to channels.
sendReminders :: HasEnv c => c -> DiscordHandler ()
sendReminders c = do
  ask >>= void . liftIO . forkIO . runReaderT loop
  where
    loop :: DiscordHandler ()
    loop = do
      (channel, message) <- liftIO $  atomically $ readTChan $ reminderChan c
      either (liftIO . print) (\_ -> pure ()) <=< restCall $ R.CreateMessage channel message
      loop
