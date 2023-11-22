{-# LANGUAGE UndecidableInstances #-}

module Conduit.DB.Init where

import Conduit.DB.Core (DBPool(..))
import Conduit.Features.Account.DB (migrateAccountTables)
import Conduit.Features.Articles.DB (createArticleFunctions, migrateArticleTables)
import Database.Esqueleto.Experimental (SqlPersistT, createPoolConfig, rawExecute, runMigration, runSqlPool)
import Database.Persist.Postgresql (PostgresConf(..))
import UnliftIO (MonadUnliftIO)

data PGConnOps = PGConnOps
  { connStr     :: !Text
  , connSize    :: !Int
  , connTimeout :: !Int
  , connStripes :: !Int
  , truncTables :: !Bool
  } deriving (Read)

mkPoolConfig :: PGConnOps -> PostgresConf
mkPoolConfig PGConnOps {..} = PostgresConf
  { pgConnStr         = fromString $ toString connStr
  , pgPoolSize        = connSize
  , pgPoolIdleTimeout = fromIntegral connTimeout
  , pgPoolStripes     = connStripes
  }

mkDBPool :: (MonadIO m) => PGConnOps -> m DBPool
mkDBPool = liftIO . fmap DBPool . createPoolConfig . mkPoolConfig

initDB :: (MonadUnliftIO m) => DBPool -> PGConnOps -> m ()
initDB (DBPool pool) ops = flip runSqlPool pool do
  when ops.truncTables resetTables
  runMigrations
  runDBFunctions

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
