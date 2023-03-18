-- | This module contains logic around sqlite-easy
module Butler.Database (
    Database,
    withDatabase,
    DatabaseMigration (..),
    dbSimpleCreate,
    dbExecute,
    dbQuery,

    -- * re-export
    NamedParam ((:=)),
    Only (..),
)
where

import Database.Migrant hiding (withTransaction)
import Database.Migrant.Driver.Sqlite ()
import Database.SQLite.Simple

import Butler.Core
import Butler.Core.Storage
import Butler.Prelude

newtype Database = Database (MVar Connection)

withDatabase :: StorageAddress -> DatabaseMigration -> (Database -> ProcessIO a) -> ProcessIO a
withDatabase addr migrations cb = do
    fp <-
        if addr == ":memory:"
            then pure ":memory:"
            else flip mappend ".sqlite" . into @FilePath . decodeUtf8 <$> getPath addr
    withRunInIO \runInIO ->
        withConnection fp \conn -> runInIO do
            db <- Database <$> newMVar conn
            dbSetup db migrations
            cb db

data DatabaseMigration = DatabaseMigration
    { migrations :: [MigrationName]
    , migrateUp :: MigrationName -> Database -> ProcessIO ()
    , migrateDown :: MigrationName -> Database -> ProcessIO ()
    }

dbSimpleCreate :: Text -> Text -> DatabaseMigration
dbSimpleCreate tableName fields = DatabaseMigration [migrationName] doUp doDown
  where
    migrationName = fromString (into @String tableName <> "-create")
    doUp name db = do
        when (name /= migrationName) (error $ "Invalid name!?: " <> show name)
        dbExecute db (Query $ "CREATE TABLE " <> tableName <> "(" <> fields <> ")") []
    doDown name db = do
        when (name /= migrationName) (error $ "Invalid name!?: " <> show name)
        dbExecute db (Query $ "DROP TABLE " <> tableName) []

dbSetup :: Database -> DatabaseMigration -> ProcessIO ()
dbSetup (Database mvConn) databaseSetup = withMVar mvConn \conn ->
    withRunInIO \runInIO ->
        migrate
            databaseSetup.migrations
            (\m d -> runInIO (databaseSetup.migrateUp m . Database =<< newMVar d))
            (\m d -> runInIO (databaseSetup.migrateDown m . Database =<< newMVar d))
            conn

dbExecute :: MonadUnliftIO m => Database -> Query -> [NamedParam] -> m ()
dbExecute (Database mvConn) q args = withMVar mvConn \conn -> liftIO (executeNamed conn q args)

dbQuery :: (MonadUnliftIO m, FromRow r) => Database -> Query -> [NamedParam] -> m [r]
dbQuery (Database mvConn) q args = withMVar mvConn \conn -> liftIO (queryNamed conn q args)
