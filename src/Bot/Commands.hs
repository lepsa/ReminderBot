module Bot.Commands where

import           Bot.Commands.Decoder
import           Bot.Commands.Types
import           Control.Concurrent.STM
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Functor           (void)
import           Data.List
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Types.DB.Reminder (getRemindersIO)
import           Data.Types.Env
import           Discord                (DiscordHandler, restCall)
import           Discord.Interactions
import           Discord.Internal.Rest
import qualified Discord.Requests       as R

data SlashCommand = SlashCommand
  { name         :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler      :: Interaction -> DiscordHandler ()
  }

slashCommands :: HasEnv c => [c -> SlashCommand]
slashCommands =
  [ ping
  , reminder
  ]

ping :: HasEnv c => c -> SlashCommand
ping _env = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr -> void . restCall $
    R.CreateInteractionResponse
      (interactionId intr)
      (interactionToken intr)
      (interactionResponseBasic  "pong")
  }

reminder :: HasEnv c => c -> SlashCommand
reminder c = SlashCommand
  { name = "reminder"
  , registration = pure $ CreateApplicationCommandChatInput
    { createName = "reminder"
    , createLocalizedName = Nothing
    , createLocalizedDescription = Nothing
    , createDescription = "Create and delete reminders"
    , createDefaultMemberPermissions = Nothing
    , createDMPermission = Nothing
    , createOptions = Just $ OptionsSubcommands
        [ OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "delete" Nothing "Delete a reminder" Nothing
            [ OptionValueString "reminder_id"   Nothing "Reminder ID"   Nothing False (Left False) Nothing Nothing
            , OptionValueString "reminder_name" Nothing "Reminder Name" Nothing False (Left False) Nothing Nothing
            ]
        , OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "set-role" Nothing "Configure the role required to manage reminders" Nothing
            [ OptionValueRole "role" Nothing "Role" Nothing True
            ]
        , OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "list" Nothing "List all reminders" Nothing []
        , OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "register" Nothing "Register a new reminder" Nothing
            [ OptionValueChannel "channel"           Nothing "Channel to remind"          Nothing True  Nothing
            , OptionValueString  "message"           Nothing "Message to send"            Nothing True  (Left False) Nothing  Nothing
            , OptionValueString  "name"              Nothing "Reminder name"              Nothing True  (Left False) Nothing  Nothing
            , OptionValueInteger "frequency_minutes" Nothing "Reminder frequency minutes" Nothing False (Left False) (pure 0) (pure 60)
            , OptionValueInteger "frequency_hours"   Nothing "Reminder frequency hours"   Nothing False (Left False) (pure 0) (pure 24)
            , OptionValueInteger "frequency_days"    Nothing "Reminder frequency days"    Nothing False (Left False) (pure 0) (pure 7)
            , OptionValueInteger "frequency_weeks"   Nothing "Reminder frequency weeks"   Nothing False (Left False) (pure 0) (pure 52)
            ]
        ]
    }
  , handler = \intr -> case intr of
    InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} } -> do
      let sendMsg = void . restCall
            . R.CreateInteractionResponse (interactionId intr) (interactionToken intr)
            . interactionResponseBasic
      case interactionGuildId intr of
        Nothing -> sendMsg "Requires a GuildID in application command"
        Just guild -> case interactionChannelId intr of
          Nothing -> sendMsg "Requires a ChannelID in application command"
          Just channelId -> do
            case optionsData input of
              Nothing -> sendMsg "No options found in application command"
              Just opts -> case decodeEither $ decodeReminder opts of
                Left e  -> sendMsg $ T.pack $ show e
                Right o -> case o of
                  RegisterReminder register -> do
                    liftIO $ atomically $ writeTChan (threads c) $ CreateReminderChan guild register channelId (getRoles $ interactionUser intr)
                    sendMsg "Registered Reminder"
                  ListReminders -> do
                    l <- liftIO $ getRemindersIO (conn c) guild
                    let format r = reminderName r <> " : " <> "ID " <> T.pack (show $ reminderId r)
                    sendMsg $ "List Reminders:\n" <>
                      mconcat (intersperse "\n" $ format <$> l)
                  DeleteReminder uuid -> do
                    liftIO $ atomically $ writeTChan (threads c) $ DeleteReminderChan guild uuid channelId (getRoles $ interactionUser intr)
                    sendMsg "Deleted Reminder"
                  SetRole role -> do
                    liftIO $ atomically $ writeTChan (threads c) $ SetPermissionChan guild role channelId (getRoles $ interactionUser intr)
                    sendMsg "Role Set"
    _ -> pure ()
  }

getRoles :: MemberOrUser -> [RoleId]
getRoles (MemberOrUser e) = either memberRoles (const []) e
