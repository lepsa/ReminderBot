{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Monad
import           Data.Text
import qualified Data.Text.IO        as TIO
import           Discord
import qualified Discord.Requests    as R
import           Discord.Types
import           UnliftIO.Concurrent
import Data.Types.App (CanAppM)
import Control.Monad.IO.Class
import Data.Types.Env
import Control.Monad.Reader
import Discord.Interactions

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: CanAppM m c e => m ()
pingpongExample = do
  cId <- asks botToken
  userFacingError <- liftIO $ runDiscord $ def
    { discordToken = "Bot " <> cId
    , discordOnEvent = eventHandler
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    } -- if you see OnLog error, post in the discord / open an issue

  liftIO $ TIO.putStrLn userFacingError
  -- userFacingError is an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (isPing m && not (fromBot m)) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10^6)
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    -- Only sent on initial startup, set up commands and the like here
  Ready apiVersion user guilds sessionId resumeGatewayUrl shard application -> pure ()
  InteractionCreate i -> case i of
    InteractionPing iId aid tok v perms -> pure ()
    _ -> pure ()
  _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
