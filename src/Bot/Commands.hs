module Bot.Commands where

import           Bot.Commands.Decoder
import           Bot.Commands.Types
import           Data.Functor         (void)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Types.Env       (HasEnv)
import           Discord              (DiscordHandler, restCall)
import           Discord.Interactions
import qualified Discord.Requests     as R

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
reminder _env = SlashCommand
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
            [ OptionValueString "reminder_id" Nothing "Reminder ID" Nothing True (Left False) Nothing Nothing
            ]
        , OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "list" Nothing "List all reminders" Nothing []
        , OptionSubcommandOrGroupSubcommand $
          OptionSubcommand "register" Nothing "Register a new reminder" Nothing
            [ OptionValueChannel "channel"           Nothing "Channel to remind"          Nothing True  Nothing
            , OptionValueString  "message"           Nothing "Message to send"            Nothing True  (Left False) Nothing  Nothing
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
      case optionsData input of
        Nothing -> sendMsg "No options found in application command"
        Just opts -> do
          case decodeEither $ decodeReminder opts of
            Left e  -> sendMsg $ T.pack $ show e
            Right o -> sendMsg $ T.pack $ show o
    _ -> pure ()
  }