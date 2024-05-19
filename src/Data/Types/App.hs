module Data.Types.App where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Types.Env
import Data.Types.Error

type AppM m c e = ReaderT c (ExceptT e m)
type CanAppM m c e =
  ( HasEnv c
  , AsError e AppError
  , MonadReader c m
  , MonadError e m
  , MonadIO m
  )

runAppM :: forall m c e a. c -> AppM m c e a -> m (Either e a)
runAppM c = runExceptT . flip runReaderT c