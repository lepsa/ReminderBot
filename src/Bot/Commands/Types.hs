module Bot.Commands.Types where

import           Bot.Commands.Decoder
import           Control.Applicative
import           Data.Text
import           Data.Types.DB.Util               ()
import           Data.UUID                        (UUID, fromText)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Discord.Interactions
import           Discord.Internal.Rest

newtype Seconds = Seconds { unSeconds :: Integer }
  deriving (Eq, Ord, Show, Num, Read, ToField, FromField)

data Reminder = Reminder
  { reminderId          :: UUID
  , reminderName        :: Text
  , reminderTimeBetween :: Seconds
  , reminderMessage     :: Text
  , reminderChannel     :: ChannelId
  , reminderGuild       :: GuildId
  , reminderCreated     :: UTCTime
  } deriving (Eq, Ord, Show)

instance FromRow Reminder where
  fromRow = Reminder
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data ReminderCommand
  = RegisterReminder Register
  | ListReminders
  | DeleteReminder (Either Text UUID)
  | SetRole RoleId
  deriving (Eq, Ord, Show)

data TimeBetween = TimeBetween
  { minutes :: Maybe Integer
  , hours   :: Maybe Integer
  , days    :: Maybe Integer
  , weeks   :: Maybe Integer
  } deriving (Eq, Ord, Show)

toSeconds :: TimeBetween -> Seconds
toSeconds t = Seconds $
    maybe 0 (*60)                (minutes t)
  + maybe 0 (\x -> x*60*60)      (hours t)
  + maybe 0 (\x -> x*60*60*24)   (days t)
  + maybe 0 (\x -> x*60*60*24*7) (weeks t)

data Register = Register
  { registerReminderName        :: Text
  , registerReminderTimeBetween :: TimeBetween
  , registerReminderMessage     :: Text
  , registerReminderChannel     :: ChannelId
  } deriving (Eq, Ord, Show)

decodeReminder :: OptionsData -> Decoder ReminderCommand
decodeReminder o =
      decodeListReminders o
  <|> RegisterReminder <$> decodeRegister o
  <|> decodeDeleteReminder o
  <|> decodeSetRole o

decodeUUID :: Text -> Decoder UUID
decodeUUID = maybe (fail "Could not decode a UUID") pure . fromText

decodeDeleteReminder :: OptionsData -> Decoder ReminderCommand
decodeDeleteReminder =
    withOptionsDataSubcommands
  $ named "delete"
  $ withOptionDataSubcommandOrGroupSubcommand deleteOpts
  where
    deleteOpts opts = do
      let options = mapNames opts
      u <- options .:? "reminder_id"   .!? withOptionDataValueString (const $ either decodeUUID decodeUUID)
      n <- options .:? "reminder_name" .!? withOptionDataValueString (const $ either pure pure)
      m <- maybe (fail "Needs either reminder_id or reminder_name") pure $ (Left <$> n) <|> (Right <$> u)
      pure $ DeleteReminder m

decodeSetRole :: OptionsData -> Decoder ReminderCommand
decodeSetRole =
    withOptionsDataSubcommands
  $ named "set-role"
  $ withOptionDataSubcommandOrGroupSubcommand roleOpts
  where
    roleOpts opts = SetRole <$> do
      mapNames opts .: "role" .! withOptionDataValueRole (const pure)

decodeListReminders :: OptionsData -> Decoder ReminderCommand
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
      Register
        <$> options .: "name"    .! withOptionDataValueString (const $ either pure pure)
        <*> pure tb
        <*> options .: "message" .! withOptionDataValueString (const $ either pure pure)
        <*> options .: "channel" .! withOptionDataValueChannel (const pure)
