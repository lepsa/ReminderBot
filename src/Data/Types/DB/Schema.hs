module Data.Types.DB.Schema where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Foldable
import           Data.List
import           Data.Types.App
import           Data.Types.Env
import           Data.Types.Error
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField


-- Create a table for tracking the schema version
createVersion :: Query
createVersion = "create table if not exists schema_version (version integer primary key not null)"

getSchemaVersion :: Query
getSchemaVersion = "select version from schema_version"

setSchema :: Query
setSchema = "update schema_version set version = ?"

-- This is run on application start to ensure that the schema_version table exists
createSchema :: (CanAppM m c e) => m ()
createSchema = do
  c <- asks conn
  liftIO $ execute_ c createVersion

newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Num)

instance FromRow Version where
  fromRow = Version <$> field

instance ToRow Version where
  toRow (Version v) = pure $ toField v

setupDatabase :: (CanAppM m c e) => m ()
setupDatabase = do
  c <- asks conn
  -- Explicitly enable foreign keys, as SQLite doesn't turn
  -- them on by default. If the database doesn't support the
  -- features we are expecting, exit the server reporting the
  -- error so that we can try again later with changes made.
  liftIO $ do
    execute_ c "PRAGMA foreign_keys = ON"
    [Only (fkEnabled :: Bool)] <- query_ c "PRAGMA foreign_keys"
    unless fkEnabled $ error "Foreign keys aren't supported in this version of SQLite. Please use a version with foreign key support."

    execute_ c "PRAGMA auto_vacuum = FULL"
    [Only (vacEnabled :: Int)] <- query_ c "PRAGMA auto_vacuum"
    case vacEnabled of
      0 -> putStrLn "auto_vacuum NONE"
      1 -> putStrLn "auto_vacuum FULL"
      2 -> putStrLn "auto_vacuum INCREMENTAL"
      n -> putStrLn $ "Unknown auto_vacuum value: " <> show n
    execute_ c "vacuum"

runMigrations :: (CanAppM m c e) => m ()
runMigrations = do
  c <- asks conn
  versions <- liftIO $ withTransaction c $ query_ c getSchemaVersion
  currentVersion <- case versions of
    []        -> pure 0 -- When there is no result from the schema_version table set a minimum value to start the process
    [version] -> pure version
    _         -> throwError_ $ DB NotFound
  liftIO $ putStrLn $ "Schema: " <> show currentVersion
  let migrationsToRun = filter (\(v, _) -> v >= currentVersion) $ sortBy comp migrations
  -- Iterate over all of the schema bumps that we have
  liftIO $ traverse_
    -- For each schema bump, run a transaction where
    -- if anything fails, the transaction aborts and
    -- the error propagates up, stopping the rest of
    -- the changes and the server starting.
    (withExclusiveTransaction c . runMigration c)
    migrationsToRun
  where
    comp (a, _) (b, _) = compare a b

runMigration :: Connection -> (Version, [Query]) -> IO ()
runMigration c (version, queries) = do
  -- Run the migration queries
  traverse_ (execute_ c) queries
  -- Bump the schema version for the current migration.
  execute c setSchema $ version + 1
  putStrLn $ "Ran migration for schema: " <> show version

-- Set the initial version of the schema to "0" so that other
-- schema bumps have something to work against. When this migration
-- is run, it will be immediately followed by a schema version bump
-- so the rest of the migration code can run properly.
migrateSchemaV0 :: [Query]
migrateSchemaV0 =
  [ "insert into schema_version (version) values (0)"
  ]

migrateSchemaV1 :: [Query]
migrateSchemaV1 =
  [ "create table reminder (id text primary key not null, name text not null, time_between integer not null, message text not null, channel integer not null, guild integer not null, created datetime not null, unique (name, guild))"
  ]

migrateSchemaV2 :: [Query]
migrateSchemaV2 =
  [ "create table reminder_last_sent (id text primary key not null, last_sent datetime not null)"
  ]

migrateSchemaV3 :: [Query]
migrateSchemaV3 =
  [ "create table permission (guild integer primary key not null, role integer not null)"
  ]

migrations :: [(Version, [Query])]
migrations =
  [ (0, migrateSchemaV0)
  , (1, migrateSchemaV1)
  , (2, migrateSchemaV2)
  , (3, migrateSchemaV3)
  ]
