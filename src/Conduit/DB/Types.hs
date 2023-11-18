{-# LANGUAGE UndecidableInstances, QuasiQuotes, TemplateHaskell #-}

module Conduit.DB.Types where

import Database.Esqueleto.Experimental (ConnectionPool, SqlPersistT, Key, runSqlPool, toSqlKey, fromSqlKey)
import Conduit.DB.Errors (DBError, catchSqlError)
import UnliftIO (MonadUnliftIO)
import Conduit.App.Has (Has, grab)
import Conduit.Utils ((-.))
import Language.Haskell.TH

type DBPool = ConnectionPool

class (Monad m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)

instance (Monad m, MonadUnliftIO m, MonadReader c m, Has DBPool c) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)
  runDB fn = grab @DBPool >>= runSqlPool fn -. catchSqlError

class SqlKey t id | t -> id, id -> t where
  sqlKey2ID :: Key t -> id
  id2sqlKey :: id -> Key t

-- just tried this for fun, very quickly realized I am nowhere near smart enough to do something like this
-- this took me over an hour.
-- help.
deriveSqlKey :: Name -> Name -> Q [Dec]
deriveSqlKey tableName keyName = do
  conName <- getConName keyName

  [d|
    instance SqlKey $(conT tableName) $(conT keyName) where
      sqlKey2ID = $(pure $ ConE conName) . fromSqlKey
      id2sqlKey $(conP conName [varP (mkName "id'")]) = toSqlKey id'
    |]

getConName :: Name -> Q Name
getConName typeName = do
  (TyConI tyCon) <- reify typeName
  
  case tyCon of
    NewtypeD _ _ _ _ (RecC    name _) _ -> pure name
    NewtypeD _ _ _ _ (NormalC name _) _ -> pure name
    NewtypeD {} -> fail "Newtype constructor not in expected format"
    _ -> fail "Expected a newtype"
