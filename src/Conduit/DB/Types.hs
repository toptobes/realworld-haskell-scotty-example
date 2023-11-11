{-# LANGUAGE UndecidableInstances #-}

module Conduit.DB.Types where

import Database.Esqueleto.Experimental (ConnectionPool, SqlPersistT, Key, runSqlPool)
import Conduit.DB.Errors (DBError, catchSqlError)
import UnliftIO (MonadUnliftIO)
import Conduit.App.Has (Has, grab)
import Conduit.Utils ((-.))

type DBPool = ConnectionPool

class (Monad m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)

instance (Monad m, MonadUnliftIO m, MonadReader c m, Has DBPool c) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)
  runDB fn = grab @DBPool >>= runSqlPool fn -. catchSqlError

class SqlKey k a where
  sqlKey2ID :: Key k -> a
  id2sqlKey :: a -> Key k
