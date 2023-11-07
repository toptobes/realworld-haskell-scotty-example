module Conduit.DB
  -- ( ConnectionOps(..)
  -- , DBPool
  -- , initSelda
  -- , queryMaybe
  -- , queryEither
  -- ) 
  where

import Conduit.Features.Account.DB (usersTable, followsTable)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.Selda (MonadSelda, tryCreateTable, SeldaError)
import Database.Selda.Backend (SeldaConnection)
import Database.Selda.PostgreSQL (PG, PGConnectInfo (..), pgOpen, seldaClose, withPostgreSQL)
import Database.Selda.Unsafe (rawStm)
import Database.Selda qualified as S
import UnliftIO.Exception (catch)
import UnliftIO (MonadUnliftIO)

type DBPool = Pool (SeldaConnection PG)

data ConnectionOps = ConnectionOps
  { connStr  :: !Text
  , connLife :: !Double
  , connMax  :: !Int
  }

initSelda :: ConnectionOps -> IO DBPool
initSelda ConnectionOps {..} = do
  withPostgreSQL connInfo initTables

  newPool $ defaultPoolConfig 
    seldaOpen
    seldaClose 
    connLife 
    connMax 
  where
    connInfo = PGConnectionString
      { pgConnectionString = connStr
      , pgSchema = Nothing
      }

    seldaOpen = pgOpen connInfo

initTables :: MonadSelda m => m ()
initTables = do
  rawStm "drop table users"
  tryCreateTable usersTable
  rawStm "drop table follows"
  tryCreateTable followsTable

toSeldaMaybe :: (MonadUnliftIO m, MonadSelda m) => m a -> m (Maybe a)
toSeldaMaybe stmt = catch @_ @SeldaError
  (Just <$> stmt)
  (pure . const Nothing)

catchSQLError :: (MonadUnliftIO m, MonadSelda m) => m a -> m (Either SQLError a)
catchSQLError stmt = catch @_ @SeldaError
  (Right <$> stmt)
  (pure . Left . selda2sqlError)

catchErrorInto :: (MonadUnliftIO m, MonadSelda m) => e -> m a -> m (Either e a)
catchErrorInto err stmt = catch @_ @SeldaError
  (Right <$> stmt)
  (pure . Left . const err)

data SQLError
  = SomeSQLError    Text
  | UniquenessError Text
  | DBError         Text
  | UnsafeError     Text

selda2sqlError :: SeldaError -> SQLError
selda2sqlError (S.DbError     err) = DBError      $ toText err
selda2sqlError (S.UnsafeError err) = UnsafeError  $ toText err
selda2sqlError (S.SqlError    err) = SomeSQLError $ toText err

-- Split on newlines, second to last line, check for username or email via regex?
-- ERROR: duplicate key value violates unique constraint \"users_username_key\"\n DETAIL: Key (username)=(name) already exists.\n"
