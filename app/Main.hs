module Main where
import           Bot                (pingpongExample)
import           Data.Aeson         (eitherDecodeFileStrict)
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Types.App     (runAppM)
import           Data.Types.Env     (mkEnv)
import           Data.Types.Error   (showAppError)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  fEnv <- eitherDecodeFileStrict . fromMaybe defaultConfigPath $ listToMaybe args
  env  <- either fail mkEnv fEnv
  e    <- runAppM env pingpongExample
  either (fail . showAppError) pure e
  where
    defaultConfigPath = "./config.json"
