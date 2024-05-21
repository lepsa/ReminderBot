module Data.Types.DB.Permissions where

import           Control.Monad.Reader
import           Data.Types.App         (CanAppM)
import           Data.Types.DB.Util     ()
import           Data.Types.Env
import           Database.SQLite.Simple
import           Discord.Internal.Rest


data Permission = Permission
  { permissionGuildId :: GuildId
  , permissionRoleId  :: RoleId
  } deriving (Eq, Ord, Show)

instance FromRow Permission where
  fromRow = Permission
    <$> field
    <*> field

setPermissionIO :: Connection -> GuildId -> RoleId -> IO ()
setPermissionIO c g r = do
  execute c "insert into permission (guild, role) values (?, ?)" (g, r)

setPermission :: CanAppM m c e => GuildId -> RoleId -> m ()
setPermission g r = do
  c <- asks conn
  liftIO $ setPermissionIO c g r

getPermissionIO :: Connection -> GuildId -> IO (Maybe Permission)
getPermissionIO c g = do
  l <- query c "select guild, role from permission where guild = ?" (Only g)
  case l of
    []    -> pure Nothing
    (p:_) -> pure $ Just p

getPermission :: CanAppM m c e => GuildId -> m (Maybe Permission)
getPermission g = do
  c <- asks conn
  liftIO $ getPermissionIO c g

updatePermissionIO :: Connection -> GuildId -> RoleId -> IO ()
updatePermissionIO c g r = do
  execute c "insert into permission (guild, role) values (?, ?) on conflict do update set role = ?" (g, r, r)

updatePermission :: CanAppM m c e => GuildId -> RoleId -> m ()
updatePermission g r = do
  c <- asks conn
  liftIO $ updatePermissionIO c g r

checkPermissionIO :: Connection -> GuildId -> [RoleId] -> IO Bool
checkPermissionIO c g rs = do
  m <- getPermissionIO c g
  pure $ case m of
    Nothing -> True
    Just p  -> permissionRoleId p `elem` rs

checkPermission :: CanAppM m c e => GuildId -> [RoleId] -> m Bool
checkPermission g rs = do
  c <- asks conn
  liftIO $ checkPermissionIO c g rs
