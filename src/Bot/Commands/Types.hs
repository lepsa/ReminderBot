{-# OPTIONS_GHC -Wno-orphans #-}
module Bot.Commands.Types where
import           Bot.Commands.Decoder
import           Control.Applicative
import           Data.Text
import           Data.UUID              (UUID, fromText, toString, fromString)
import           Database.SQLite.Simple
import           Discord.Interactions
import           Discord.Internal.Rest
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow
import Control.Monad

-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.
instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance FromRow UUID where
  fromRow = fieldWith fromField
instance ToField UUID where
  toField = toField @String . toString

instance FromField Snowflake where
  fromField f = Snowflake <$> fromField f

instance ToField Snowflake where
  toField = toField . unSnowflake

instance FromField (DiscordId a) where
  fromField f = DiscordId <$> fromField f

instance ToField (DiscordId a) where
  toField = toField . unId

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
