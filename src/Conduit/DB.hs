{-# LANGUAGE UndecidableInstances #-}

module Conduit.DB
  -- ( ConnectionOps(..)
  -- , DBPool
  -- , initSelda
  -- , queryMaybe
  -- , queryEither
  -- ) 
  where

import Conduit.App.Has (Has, grab)
import Database.Persist.Postgresql (PostgresConf(..))
import UnliftIO (MonadUnliftIO)
import Database.Esqueleto.Experimental (ConnectionPool, SqlPersistT, runSqlPool, createPoolConfig, rawExecute, runMigration)
import Conduit.Features.Account.DB (migrateAccountTables)
import Database.PostgreSQL.Simple (SqlError(..))
import UnliftIO.Exception (catch)
import Data.List (stripPrefix)
import Conduit.Utils ((-.))
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

type DBPool = ConnectionPool

class (Monad m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)

instance (Monad m, MonadUnliftIO m, MonadReader c m, Has DBPool c) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)
  runDB fn = grab @DBPool >>= runSqlPool fn -. catchSqlError

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

mkDBPool :: (MonadIO m) => ConnectionOps -> m ConnectionPool
mkDBPool = liftIO . createPoolConfig . mkPoolConfig

dropTables :: (MonadUnliftIO m) => DBPool -> m ()
dropTables pool = flip runSqlPool pool do
  rawExecute "drop table if exists \"user\" cascade" []
  rawExecute "drop table if exists \"follow\"" []

resetTables :: (MonadUnliftIO m) => DBPool -> m ()
resetTables pool = flip runSqlPool pool do
  rawExecute "truncate \"user\" cascade" []
  rawExecute "truncate \"follow\" cascade" []

runMigrations :: (MonadUnliftIO m) => DBPool -> m ()
runMigrations pool = flip runSqlPool pool do
  runMigration migrateAccountTables

data DBError
  = SomeDBError SqlError
  | UniquenessError Text
  deriving (Show)

catchSqlError :: (MonadUnliftIO m) => m a -> m (Either DBError a)
catchSqlError stmt = catch @_ @SqlError
  (Right <$> stmt)
  (pure . Left . mapSqlError)

mapSqlError :: SqlError -> DBError
mapSqlError err
  | err.sqlState == "23505" = UniquenessError ""
  | otherwise = SomeDBError err

-- SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"unique_username\"", sqlErrorDetail = "Key (username)=(username) already exists.", sqlErrorHint = ""}

extractUniquenessViolation :: SqlError -> Text
extractUniquenessViolation = toText . extractViolatedColName . decodeUtf8 . sqlErrorDetail
  where extractViolatedColName = extractKeyField -. fromMaybe (error "")

extractKeyField :: String -> Maybe String
extractKeyField str = do
  rest <- stripPrefix "Key (" str
  let (keyField, _) = break (== ')') rest
  Just keyField

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)
