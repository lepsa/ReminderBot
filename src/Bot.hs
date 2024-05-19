{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Data.Types.App                     (CanAppM)
import           Data.Types.Env
import           Discord
import           Discord.Interactions
import qualified Discord.Internal.Rest.Interactions as RI
import qualified Discord.Requests                   as R
import           Discord.Types
import Data.Traversable
import Data.Foldable

type DiscordM c = ReaderT c DiscordHandler
runDiscordM :: c -> DiscordM c a -> DiscordHandler a
runDiscordM c m = runReaderT m c
type CanDiscord m c = (MonadReader c m)

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: CanAppM m c e => m ()
pingpongExample = do
  cId <- asks botToken
  c <- ask
  userFacingError <- liftIO $ runDiscord $ def
    { discordToken = "Bot " <> cId
    , discordOnEvent = runDiscordM c . eventHandler
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    } -- if you see OnLog error, post in the discord / open an issue

  liftIO $ TIO.putStrLn userFacingError
  -- userFacingError is an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordM c ()
eventHandler event = case event of
  MessageCreate m -> lift $ when (isPing m && not (fromBot m)) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    liftIO $ threadDelay (2 * 10^6)
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    -- Only sent on initial startup, set up commands and the like here
  Ready apiVersion user guilds sessionId resumeGatewayUrl shard (PartialApplication appId _) -> lift $ onReady appId $ idOnceAvailable <$> guilds
  InteractionCreate i -> lift $ onInteractionCreate i
  _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent

registerCommands :: DiscordM c ()
registerCommands = pure ()

data SlashCommand = SlashCommand
  { name         :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler      :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

mySlashCommands :: [SlashCommand]
mySlashCommands = [ping]

ping :: SlashCommand
ping = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic  "pong")
  }

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show

onReady :: ApplicationId -> [GuildId] -> DiscordHandler ()
onReady appId guilds = do
  echo "Bot ready!"
  appCmdRegistrations <- for guilds $ \guild -> do
    l <- traverse (tryRegistering guild) mySlashCommands
    pure (guild, l)

  for_ appCmdRegistrations $ \(guild, appCmdRegistrations') ->
    case sequence appCmdRegistrations' of
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
         in forM_ outdatedIds $
              restCall . R.DeleteGuildApplicationCommand appId guild


onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate cmd = case cmd of
  InteractionPing iId _aid tok _v _perms -> void $ restCall $
    RI.CreateInteractionResponse iId tok InteractionResponsePong
  InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == name c) mySlashCommands
      of
        Just found ->
          handler found cmd (optionsData input)

        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"
  _ ->
    pure () -- Unexpected/unsupported interaction type

