{-# LANGUAGE UndecidableInstances, QuasiQuotes, TemplateHaskell #-}

module Conduit.DB.Types
  ( DBPool(DBPool)
  , MonadDB(..)
  , SqlKey(..)
  , deriveSqlKey
  ) where

import Conduit.App.Has (Has, grab)
import Conduit.DB.Errors (DBError, catchSqlError)
import Conduit.Utils ((-.))
import Database.Esqueleto.Experimental (ConnectionPool, Key, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import Language.Haskell.TH
import UnliftIO (MonadUnliftIO)

-- | The 'ConnectionPool' managing the DB connections.
newtype DBPool = DBPool { unPool :: ConnectionPool }

-- | Some monad which can run an Esqueleto SQL query/stmt.
class (Monad m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)

instance (Monad m, MonadUnliftIO m, Has DBPool c m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)
  runDB fn = grab @DBPool <&> unPool >>= runSqlPool fn -. catchSqlError

-- | An abstraction to allow for easy conversion between Esqueleto entity Keys and Conduit's own ID datatypes.
--   'deriveSqlKey' can automagically create instances for this class.
--   
-- > newtype TableID = TableID { unID :: Int64 }
-- > <define some persist table Table>
-- > $(deriveSqlKey ''Table ''TableID)
class SqlKey t id | t -> id, id -> t where
  sqlKey2ID :: Key t -> id
  id2sqlKey :: id -> Key t

-- | just tried this for fun, very quickly realized I am nowhere near smart enough to be doing something like this.
--   this took me over an hour.
--   help.
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
