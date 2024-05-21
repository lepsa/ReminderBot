module Bot.Commands.Types where
import           Bot.Commands.Decoder
import           Control.Applicative
import           Data.Scientific
import           Data.Text
import           Data.UUID             (UUID, fromText)
import           Discord.Interactions
import           Discord.Internal.Rest

data Reminder
  = RegisterReminder Register
  | ListReminders
  | DeleteReminder UUID
  deriving (Eq, Ord, Show)

data TimeBetween = TimeBetween
  { minutes :: Maybe Integer
  , hours   :: Maybe Integer
  , days    :: Maybe Integer
  , weeks   :: Maybe Integer
  } deriving (Eq, Ord, Show)

data Register = Register
  { registerReminderTimeBetween :: TimeBetween
  , registerReminderMessage     :: Text
  , registerReminderChannel     :: ChannelId
  } deriving (Eq, Ord, Show)

decodeReminder :: OptionsData -> Decoder Reminder
decodeReminder o =
      decodeListReminders o
  <|> RegisterReminder <$> decodeRegister o
  <|> decodeDeleteReminder o

decodeDeleteReminder :: OptionsData -> Decoder Reminder
decodeDeleteReminder =
    withOptionsDataSubcommands
  $ named "delete"
  $ withOptionDataSubcommandOrGroupSubcommand deleteOpts
  where
    deleteOpts opts = do
      let options = mapNames opts
      uuid <- options .: "reminder_id" .! withOptionDataValueString (const $ either pure pure)
      maybe
        (fail "Could not decode a UUID")
        (pure . DeleteReminder)
        $ fromText uuid

decodeListReminders :: OptionsData -> Decoder Reminder
decodeListReminders =
    withOptionsDataSubcommands
  $ named "list"
  $ const $ pure ListReminders

decodeRegister :: OptionsData -> Decoder Register
decodeRegister =
    withOptionsDataSubcommands
  $ named "register"
  $ withOptionDataSubcommandOrGroupSubcommand registerOpts
  where
    registerOpts opts = do
      let options = mapNames opts
      tb <- TimeBetween
        <$> options .:? "frequency_minutes" .!? withOptionDataValueInteger (const eitherDecoder)
        <*> options .:? "frequency_hours"   .!? withOptionDataValueInteger (const eitherDecoder)
        <*> options .:? "frequency_days"    .!? withOptionDataValueInteger (const eitherDecoder)
        <*> options .:? "frequency_weeks"   .!? withOptionDataValueInteger (const eitherDecoder)
      Register tb
        <$> options .: "message" .! withOptionDataValueString (const $ either pure pure)
        <*> options .: "channel" .! withOptionDataValueChannel (const pure)

data FooSubcommand
  = Frstsubcmdgrp Text Integer
  | Frstsubcmd Text
  | Sndsubcmd Bool (Maybe Scientific) (Maybe Integer) (Maybe Integer) (Maybe UserId) (Maybe ChannelId) (Maybe Snowflake)
  deriving (Eq, Ord, Show)

decodeFooSubcommandFrstsubcmdgrp :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandFrstsubcmdgrp =
    withOptionsDataSubcommands
  $ named "frstsubcmdgrp"
  $ named "frstsubcmd" scOptions
  where
    scOptions opts = do
      let options = mapNames opts
      Frstsubcmdgrp
        <$> options .: "onestringinput" .! withOptionDataValueString  (const eitherDecoder)
        <*> options .: "oneintinput"    .! withOptionDataValueInteger (const eitherDecoder)

decodeFooSubcommandFrstsubcmd :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandFrstsubcmd =
      withOptionsDataSubcommands
    $ named "frstsubcmd"
    $ withOptionDataSubcommandOrGroupSubcommand
    $ named "onestringinput"
    $ withOptionDataValueString
    $ \_ -> fmap Frstsubcmd . eitherDecoder

decodeFooSubcommandSndsubcmd :: OptionsData -> Decoder FooSubcommand
decodeFooSubcommandSndsubcmd =
    withOptionsDataSubcommands
  $ named "sndsubcmd"
  $ withOptionDataSubcommandOrGroupSubcommand scOptions
  where
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
