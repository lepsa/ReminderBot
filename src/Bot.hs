{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Bot.Commands
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Data.Traversable
import           Data.Types.App                     (CanAppM)
import           Data.Types.Env
import           Discord
import           Discord.Interactions
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
  cId <- asks botToken
  c <- ask
  userFacingError <- liftIO $ runDiscord $ def
    { discordToken = "Bot " <> cId
    , discordOnEvent = eventHandler c
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
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
      guildCmdRegistrations <- for guilds $ \guild -> do
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
          Just found -> handler found cmd (optionsData input)
          Nothing -> echo "Somehow got unknown slash command (registrations out of date?)"
      _ -> pure () -- Unexpected/unsupported interaction type

