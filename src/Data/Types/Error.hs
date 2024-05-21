module Data.Types.Error where
import           Control.Monad.Except

data AppError
  = DB DbError
  | Other String
  deriving (Eq, Ord, Show)

data DbError
  = NotFound
  | TooManyResults
  | FailedToInsertRecord
  deriving (Eq, Ord, Show)

showAppError :: AppError -> String
showAppError (Other s) = s
showAppError (DB e)    = showDbError e

showDbError :: DbError -> String
showDbError NotFound             = "Not Found"
showDbError TooManyResults       = "Too Many Results"
showDbError FailedToInsertRecord = "Failed To Insert Record"

class AsError e where
  fromError :: AppError -> e
  toError :: e -> Maybe AppError

-- All errors can map to themselves.
instance AsError AppError where
  fromError = id
  toError = pure

liftEither_ :: (MonadError e m, AsError e) => Either AppError a -> m a
liftEither_ = either (throwError . fromError) pure

throwError_ :: (MonadError e m, AsError e) => AppError -> m a
throwError_ = throwError . fromError
