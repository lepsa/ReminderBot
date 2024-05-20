module Bot.Commands where

import           Bot.Commands.Decoder
import           Control.Applicative    (Alternative (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Functor           (void)
import           Data.Scientific
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Types.Env         (HasEnv)
import           Discord                (DiscordHandler, restCall)
import           Discord.Interactions
import           Discord.Internal.Rest  (ChannelId, Snowflake, UserId)
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
  , exampleSlashCommand
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

exampleSlashCommand :: HasEnv c => c -> SlashCommand
exampleSlashCommand _env = SlashCommand
  { name = cmdName
  , registration = pure $ newExampleSlashCommand cmdName
  , handler = \intr _options ->
    case intr of
      InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
        void $ restCall $ R.CreateInteractionResponse (interactionId intr) (interactionToken intr) $ interactionResponseBasic $
          -- T.pack $ "oh boy, subcommands! welp, here's everything I got from that: " <> show (optionsData input)
          T.pack $
            maybe "No options data"
            (either id show . decodeEither . decodeFooSubCommand)
            $ optionsData input
      _ -> pure ()
  }
  where
    cmdName = "reminder"

data FooSubcommand
  = Frstsubcmdgrp Text Integer
  | Frstsubcmd Text
  | Sndsubcmd Bool (Maybe Scientific) (Maybe Integer) (Maybe Integer) (Maybe UserId) (Maybe ChannelId) (Maybe Snowflake)
  deriving (Eq, Ord, Show)

eitherDecoder :: Either Text a -> Decoder a
eitherDecoder = either failText pure

decodeFooSubcommandFrstsubcmdgrp :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandFrstsubcmdgrp = withOptionsDataSubcommands $
  frstsubcmdgrp (frstsubcmd scOptions)
  where
    frstsubcmdgrp :: (OptionDataSubcommandOrGroup -> Decoder a) -> [OptionDataSubcommandOrGroup] -> Decoder a
    frstsubcmdgrp = named "frstsubcmdgrp"
    frstsubcmd :: (OptionDataSubcommand -> Decoder a) -> OptionDataSubcommandOrGroup -> Decoder a
    frstsubcmd = named "frstsubcmd"
    scOptions opts = do
      let options = mapNames opts
      Frstsubcmdgrp
        <$> options .: "onestringinput" .! withOptionDataValueString  (const eitherDecoder)
        <*> options .: "oneintinput"    .! withOptionDataValueInteger (const eitherDecoder)

decodeFooSubcommandFrstsubcmd :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandFrstsubcmd = withOptionsDataSubcommands $
  frstsubcmd onestringinput
  where
    frstsubcmd :: (OptionDataSubcommand -> Decoder a) -> [OptionDataSubcommandOrGroup] -> Decoder a
    frstsubcmd f = named "frstsubcmd" (withOptionDataSubcommandOrGroupSubcommand f)
    onestringinput :: OptionDataSubcommand -> Decoder FooSubcommand
    onestringinput = named "onestringinput" stringInput
    stringInput :: OptionDataValue -> Decoder FooSubcommand
    stringInput = withOptionDataValueString $ \_ e -> Frstsubcmd <$>
      either (fail . T.unpack) pure e

decodeFooSubcommandSndsubcmd :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandSndsubcmd = withOptionsDataSubcommands $
  sndsubcmd scOptions
  where
    sndsubcmd :: (OptionDataSubcommand -> Decoder a) -> [OptionDataSubcommandOrGroup] -> Decoder a
    sndsubcmd f = named "sndsubcmd" (withOptionDataSubcommandOrGroupSubcommand f)
    scOptions :: OptionDataSubcommand -> Decoder FooSubcommand
    scOptions o = do
      let m = mapNames o
      Sndsubcmd
        <$> m .:  "trueorfalse" .!  withOptionDataValueBoolean     (const pure)
        <*> m .:? "numbercomm"  .!? withOptionDataValueNumber      (const eitherDecoder)
        <*> m .:? "numbercomm2" .!? withOptionDataValueInteger     (const eitherDecoder)
        <*> m .:? "numbercomm3" .!? withOptionDataValueInteger     (const eitherDecoder)
        <*> m .:? "user"        .!? withOptionDataValueUser        (const pure)
        <*> m .:? "channel"     .!? withOptionDataValueChannel     (const pure)
        <*> m .:? "mentionable" .!? withOptionDataValueMentionable (const pure)

decodeFooSubCommand :: OptionsData -> Decoder FooSubcommand
decodeFooSubCommand o =
      decodeFooSubcommandFrstsubcmdgrp o
  <|> decodeFooSubcommandFrstsubcmd o
  <|> decodeFooSubcommandSndsubcmd o

-- | Example slash command that has subcommands and multiple types of fields.
newExampleSlashCommand :: Text -> CreateApplicationCommand
newExampleSlashCommand n = CreateApplicationCommandChatInput
  { createName = n
  , createLocalizedName = Nothing
  , createLocalizedDescription = Nothing
  , createDescription = "testing out subscommands"
  , createDefaultMemberPermissions = Nothing
  , createDMPermission = Nothing
  , createOptions = Just
        ( OptionsSubcommands
            [ OptionSubcommandGroup
              "frstsubcmdgrp"
              Nothing
              "the sub command group"
              Nothing
              [ OptionSubcommand
                "frstsubcmd"
                Nothing
                "the first sub sub command"
                Nothing
                [ OptionValueString
                  "onestringinput"
                  Nothing
                  "two options"
                  Nothing
                  True
                  ( Right
                    [ Choice "green" Nothing "green"
                    , Choice "red" Nothing "red"
                    ]
                  )
                  Nothing
                  Nothing,
                  OptionValueInteger "oneintinput" Nothing "choices galore" Nothing False (Left False) Nothing Nothing
                ]
              ]
            , OptionSubcommandOrGroupSubcommand $
                OptionSubcommand
                  "frstsubcmd"
                  Nothing
                  "the first subcommand"
                  Nothing
                  [ OptionValueString
                    "onestringinput"
                    Nothing
                    "two options"
                    Nothing
                    True
                    ( Right
                      [ Choice "yellow" Nothing "yellow",
                        Choice "blue" Nothing "blue"
                      ]
                    )
                    Nothing
                    Nothing
                  ]
            , OptionSubcommandOrGroupSubcommand $
                OptionSubcommand
                  "sndsubcmd"
                  Nothing
                  "the second subcommand"
                  Nothing
                  [ OptionValueBoolean
                      "trueorfalse"
                      Nothing
                      "true or false"
                      Nothing
                      True,
                    OptionValueNumber
                      "numbercomm"
                      Nothing
                      "number option"
                      Nothing
                      False
                      (Left True)
                      (Just 3.1415)
                      (Just 101),
                    OptionValueInteger
                      "numbercomm2"
                      Nothing
                      "another number option"
                      Nothing
                      False
                      (Right [Choice "one" Nothing 1, Choice "two" Nothing 2, Choice "minus 1" Nothing (-1)])
                      (Just $ -1)
                      (Just $ -2),
                    OptionValueInteger
                      "numbercomm3"
                      Nothing
                      "another another number option"
                      Nothing
                      False
                      (Left True)
                      (Just $ -50)
                      (Just 50),
                    OptionValueUser
                      "user"
                      Nothing
                      "testing asking for a user"
                      Nothing
                      False,
                    OptionValueChannel
                      "channel"
                      Nothing
                      "testing asking for a channel"
                      Nothing
                      False
                      (Just [ChannelTypeOptionGuildVoice]),
                    OptionValueMentionable
                      "mentionable"
                      Nothing
                      "testing asking for a mentionable"
                      Nothing
                      False
                  ]
            ]
        )
      }
