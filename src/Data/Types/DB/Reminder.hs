module Data.Types.DB.Reminder where

import           Bot.Commands.Types
import           Control.Monad.Reader
import           Data.Text
import           Data.Types.App         (CanAppM)
import           Data.Types.DB.Util     (ensureSingleInsert, ensureSingleResult)
import           Data.Types.Env
import           Data.UUID              (UUID)
import           Data.UUID.V4           (nextRandom)
import           Database.SQLite.Simple
import           Discord.Internal.Types

getReminderByName :: CanAppM m c e => GuildId -> Text -> m Reminder
getReminderByName guild name = do
  c <- asks conn
  l <- liftIO $ query c "select id, name, time_between, message, channel, guild, created from reminder where guild = ? and name = ?" (guild, name)
  ensureSingleResult l

getReminderById :: CanAppM m c e => GuildId -> UUID -> m Reminder
getReminderById guild uuid = do
  c <- asks conn
  l <- liftIO $ query c "select id, name, time_between, message, channel, guild, created from reminder where guild = ? and id = ?" (guild, uuid)
  ensureSingleResult l

getReminders :: CanAppM m c e => m [Reminder]
getReminders = do
  c <- asks conn
  liftIO $ getRemindersIO c

getRemindersIO :: Connection -> IO [Reminder]
getRemindersIO c = query_ c "select id, name, time_between, message, channel, guild, created from reminder"

createReminder :: CanAppM m c e => Text -> Seconds -> Text -> ChannelId -> GuildId -> m Reminder
createReminder name timeBetween message channel guild = do
  c <- asks conn
  uuid <- liftIO nextRandom
  l <- liftIO $ query c "insert into reminder (id, name, time_between, message, channel, guild, created) values (?, ?, ?, ?, ?, ?, datetime()) returning id, name, time_between, message, channel, guild, created" (uuid, name, timeBetween, message, channel, guild)
  ensureSingleInsert l

deleteReminderById :: CanAppM m c e => GuildId -> UUID -> m ()
deleteReminderById guild uuid = do
  c <- asks conn
  liftIO $ deleteReminderByIdIO c guild uuid

deleteReminderByIdIO :: Connection -> GuildId -> UUID -> IO ()
deleteReminderByIdIO c guild uuid = do
  execute c "delete from reminder where guild = ? and id = ?" (guild, uuid)
  execute c "delete from reminder_last_sent where id = ?" (Only uuid)

deleteReminderByName :: CanAppM m c e => GuildId -> Text -> m ()
deleteReminderByName guild name = do
  c <- asks conn
  liftIO $ deleteReminderByNameIO c guild name

deleteReminderByNameIO :: Connection -> GuildId -> Text -> IO ()
deleteReminderByNameIO c guild name = do
  l <- query c "select id from reminder where guild = ? and name = ?" (guild, name)
  case l of
    []       -> pure ()
    (uuid:_) -> deleteReminderByIdIO c guild uuid

deleteReminderByRef :: CanAppM m c e => GuildId -> Either Text UUID -> m ()
deleteReminderByRef guild ref = do
  c <- asks conn
  liftIO $ deleteReminderByRefIO c guild ref

deleteReminderByRefIO :: Connection -> GuildId -> Either Text UUID -> IO ()
deleteReminderByRefIO c guild = either (deleteReminderByNameIO c guild) (deleteReminderByIdIO c guild)

setLastSent :: CanAppM m c e => UUID -> UTCTime -> m ()
setLastSent uuid time = do
  c <- asks conn
  liftIO $ setLastSentIO c uuid time

setLastSentIO :: Connection -> UUID -> UTCTime -> IO ()
setLastSentIO c uuid time =
  execute c "insert into reminder_last_sent (id, last_sent) values (?, ?) on conflict do update set last_sent = ?" (uuid, time, time)

getLastSent :: CanAppM m c e => UUID -> m (Maybe UTCTime)
getLastSent uuid = do
  c <- asks conn
  liftIO $ getLastSentIO c uuid

getLastSentIO :: Connection -> UUID -> IO (Maybe UTCTime)
getLastSentIO c uuid = do
  l <- query c "select last_sent from reminder_last_sent where id = ?" (Only uuid)
  case l of
    []         -> pure Nothing
    (Only t:_) -> pure $ Just t

deleteLastSentIO :: Connection -> UUID -> IO ()
deleteLastSentIO c uuid = execute c "delete from reminder_last_sent where id = ?" (Only uuid)

deleteLastSent :: CanAppM m c e => UUID -> m ()
deleteLastSent uuid = do
  c <- asks conn
  liftIO $ deleteLastSentIO c uuid
