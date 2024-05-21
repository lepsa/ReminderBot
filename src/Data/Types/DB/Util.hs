{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Types.DB.Util where

import Data.Types.Error
import Control.Monad.Except
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow
import Control.Monad
import Data.UUID
import Discord.Internal.Rest

ensureSingleInsert :: (AsError e, MonadError e m) => [a] -> m a
ensureSingleInsert = either (throwError . fromError) pure . ensureSingleInsert'

ensureSingleResult :: (AsError e, MonadError e m) => [a] -> m a
ensureSingleResult = either (throwError . fromError) pure . ensureSingleResult'

ensureSingleResult' :: [a] -> Either AppError a
ensureSingleResult' []  = Left $ DB NotFound
ensureSingleResult' [a] = pure a
ensureSingleResult' _   = Left $ DB TooManyResults

ensureSingleInsert' :: [a] -> Either AppError a
ensureSingleInsert' []  = Left $ DB FailedToInsertRecord
ensureSingleInsert' [a] = pure a
ensureSingleInsert' _   = Left $ Other "insert query returned multiple rows"

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