module Data.Types.Env where

import           Data.Aeson
import           Data.Text
import           Database.SQLite.Simple

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

data Env = Env
  { envConn         :: Connection
  , envClientId     :: Text
  , envClientSecret :: Text
  , envPublicKey    :: Text
  , envBotToken     :: Text
  }

class HasEnv c where
  {-# MINIMAL env | (conn, clientId, clientSecret, publicKey, botToken) #-}
  env :: c -> Env
  env c = Env (conn c) (clientId c) (clientSecret c) (publicKey c) (botToken c)
  conn :: c -> Connection
  conn = conn . env
  clientId :: c -> Text
  clientId = clientId . env
  clientSecret :: c -> Text
  clientSecret = clientSecret . env
  publicKey :: c -> Text
  publicKey = publicKey . env
  botToken :: c -> Text
  botToken = botToken . env

instance HasEnv Env where
  env = id
  conn = envConn
  clientId = envClientId
  clientSecret = envClientSecret
  publicKey = envPublicKey
  botToken = envBotToken
