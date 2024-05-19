module Data.Types.Error where

data AppError
  = AppError
  | DB DbError
  deriving (Eq, Ord)

data DbError = DbError
  deriving (Eq, Ord)

showAppError :: AppError -> String
showAppError AppError = "AppError"
showAppError (DB e) = showDbError e

showDbError :: DbError -> String
showDbError DbError = "DbError"

class AsError to from where
  fromError :: from -> to
  toError :: to -> Maybe from

-- All errors can map to themselves.
instance AsError e e where
  fromError = id
  toError = pure
