{-# LANGUAGE UndecidableInstances #-}

module Conduit.DB.Init where

import Conduit.DB.Types (DBPool)
import Conduit.Features.Account.DB (migrateAccountTables)
import Database.Esqueleto.Experimental (createPoolConfig, rawExecute, runMigration, runSqlPool, SqlPersistT)
import Database.Persist.Postgresql (PostgresConf(..))
import UnliftIO (MonadUnliftIO)
import Conduit.Features.Articles.DB (createArticleFunctions, migrateArticleTables)

data ConnectionOps = ConnectionOps
  { connStr     :: !Text
  , connSize    :: !Int
  , connTimeout :: !Int
  , connStripes :: !Int
  } deriving (Read)

mkPoolConfig :: ConnectionOps -> PostgresConf
mkPoolConfig ConnectionOps {..} = PostgresConf
  { pgConnStr         = fromString $ toString connStr
  , pgPoolSize        = connSize
  , pgPoolIdleTimeout = fromIntegral connTimeout
  , pgPoolStripes     = connStripes
  }

mkDBPool :: (MonadIO m) => ConnectionOps -> m DBPool
mkDBPool = liftIO . createPoolConfig . mkPoolConfig

initDB :: (MonadUnliftIO m) => DBPool -> m ()
initDB pool = flip runSqlPool pool do
  runMigrations
  runDBFunctions
  resetTables

tables :: [Text]
tables = ["user", "follow", "article", "favorite", "comment"]

dropTables :: (MonadIO m) => SqlPersistT m ()
dropTables = flip rawExecute [] $
  foldMap (\t -> "drop table if exists  \"" <> t <> "\" cascade;") tables

resetTables :: (MonadIO m) => SqlPersistT m ()
resetTables = flip rawExecute [] $
  foldMap (\t -> "truncate \"" <> t <> "\" cascade;") tables

runMigrations :: (MonadIO m) => SqlPersistT m ()
runMigrations = do
  runMigration migrateAccountTables
  runMigration migrateArticleTables

runDBFunctions :: (MonadIO m) => SqlPersistT m ()
runDBFunctions = do
  createArticleFunctions
