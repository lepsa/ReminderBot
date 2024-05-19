module Bot.Commands where

import           Control.Monad.IO.Class
import           Data.Functor           (void)
import           Data.Text              (Text)
import           Data.Types.Env         (HasEnv)
import           Discord                (DiscordHandler, restCall)
import           Discord.Interactions   (CreateApplicationCommand,
                                         Interaction (interactionId, interactionToken),
                                         OptionsData, createChatInput,
                                         interactionResponseBasic)
import qualified Discord.Requests       as R

data SlashCommand = SlashCommand
  { name         :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler      :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

slashCommands :: HasEnv c => [c -> SlashCommand]
slashCommands =
  [ ping
  , registerReminder
  ]

ping :: HasEnv c => c -> SlashCommand
ping _env = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic  "pong")
  }

registerReminder :: HasEnv c => c -> SlashCommand
registerReminder _env = SlashCommand
  { name = "register-reminder"
  , registration = createChatInput "register-reminder" "Creates a new reminder"
  , handler = \intr options -> do
      liftIO $ print options
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic "Registered!")
  }
