module Data.Types.Env where

import           Bot.Commands.Types     (Register)
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Text
import           Data.UUID              (UUID)
import           Database.SQLite.Simple
import           Discord.Internal.Rest

data FileEnv = FileEnv
  { fileEnvClientId     :: Text
  , fileEnvClientSecret :: Text
  , fileEnvPublicKey    :: Text
  , fileEnvBotToken     :: Text
  , fileEnvDbName       :: FilePath
  }

instance FromJSON FileEnv where
  parseJSON = withObject "FileEnv" $ \o -> FileEnv
    <$> o .: "clientId"
    <*> o .: "clientSecret"
    <*> o .: "publicKey"
    <*> o .: "botToken"
    <*> o .: "databaseName"

mkEnv :: FileEnv -> IO Env
mkEnv fileEnv = Env
  <$> open (fileEnvDbName fileEnv)
  <*> pure (fileEnvClientId fileEnv)
  <*> pure (fileEnvClientSecret fileEnv)
  <*> pure (fileEnvPublicKey fileEnv)
  <*> pure (fileEnvBotToken fileEnv)
  <*> newTChanIO
  <*> newTChanIO
  <*> newTVarIO []

data CreateDeleteReminder
  = CreateReminderChan (Maybe GuildId) (Maybe ChannelId) Register
  | DeleteReminderChan (Maybe GuildId) (Maybe ChannelId) UUID
  -- Actions for server startup and stop
  | InitialiseReminder
  | StopAll
  deriving (Eq, Ord, Show)

data Env = Env
  { envConn         :: Connection
  , envClientId     :: Text
  , envClientSecret :: Text
  , envPublicKey    :: Text
  , envBotToken     :: Text
  , envThreads      :: TChan CreateDeleteReminder
  , envReminderChan :: TChan (ChannelId, Text)
  , envReminderIds  :: TVar [UUID]
  }

class HasEnv c where
  {-# MINIMAL env | (conn, clientId, clientSecret, publicKey, botToken, threads, reminderChan, reminderIds) #-}
  env :: c -> Env
  env c = Env
    (conn c) (clientId c) (clientSecret c) (publicKey c)
    (botToken c) (threads c) (reminderChan c) (reminderIds c)
  conn :: c -> Connection
  conn = envConn . env
  clientId :: c -> Text
  clientId = envClientId . env
  clientSecret :: c -> Text
  clientSecret = envClientSecret . env
  publicKey :: c -> Text
  publicKey = envPublicKey . env
  botToken :: c -> Text
  botToken = envBotToken . env
  threads :: c -> TChan CreateDeleteReminder
  threads = envThreads . env
  reminderChan :: c -> TChan (ChannelId, Text)
  reminderChan = envReminderChan . env
  reminderIds :: c -> TVar [UUID]
  reminderIds = envReminderIds . env

instance HasEnv Env where
  env = id
