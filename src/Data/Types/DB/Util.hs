module Data.Types.DB.Util where

import Data.Types.Error
import Control.Monad.Except

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
