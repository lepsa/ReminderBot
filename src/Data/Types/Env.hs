module Data.Types.Env where

import           Data.Aeson
import           Data.Text
import           Database.SQLite.Simple

data FileEnv = FileEnv
  { fileEnvClientId :: Text
  , fileEnvClientSecret :: Text
  , fileEnvBotToken :: Text
  , fileEnvDbName :: FilePath
  }

instance FromJSON FileEnv where
  parseJSON = withObject "FileEnv" $ \o -> FileEnv
    <$> o .: "clientId"
    <*> o .: "clietnSecret"
    <*> o .: "botToken"
    <*> o .: "databaseName"

mkEnv :: FileEnv -> IO Env
mkEnv fileEnv = Env
  <$> open (fileEnvDbName fileEnv)
  <*> pure (fileEnvClientId fileEnv)
  <*> pure (fileEnvClientSecret fileEnv)
  <*> pure (fileEnvBotToken fileEnv)

data Env = Env
  { envConn     :: Connection
  , envClientId :: Text
  , envClientSecret :: Text
  , envBotToken :: Text
  }

class HasEnv c where
  {-# MINIMAL env | (conn, clientId, clientSecret, botToken) #-}
  env :: c -> Env
  env c = Env (conn c) (clientId c) (clientSecret c) (botToken c)
  conn :: c -> Connection
  conn = conn . env
  clientId :: c -> Text
  clientId = clientId . env
  clientSecret :: c -> Text
  clientSecret = clientSecret . env
  botToken :: c -> Text
  botToken = botToken . env

instance HasEnv Env where
  env = id
  conn = envConn
  clientId = envClientId
