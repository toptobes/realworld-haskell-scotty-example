{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, TemplateHaskell #-}

module Conduit.DB.Core where

import Conduit.App.Has (Has, grab)
import Conduit.Errors (FeatureError(..))
import Conduit.Utils ((.-))
import Data.List (stripPrefix)
import Database.Esqueleto.Experimental (ConnectionPool, Key, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import Database.PostgreSQL.Simple (ExecStatus(..), SqlError(..))
import Language.Haskell.TH
import UnliftIO (MonadUnliftIO, catch)

-- | The 'ConnectionPool' managing the DB connections.
newtype DBPool = DBPool { unPool :: ConnectionPool }

-- | Some monad which can run an Esqueleto SQL query/stmt.
class (Monad m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)

instance (Monad m, MonadUnliftIO m, Has DBPool c m) => MonadDB m where
  runDB :: SqlPersistT m a -> m (Either DBError a)
  runDB fn = grab @DBPool <&> unPool >>= runSqlPool fn .- catchSqlError

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
-- 
-- see 'SqlKey' for usage
deriveSqlKey :: Name -> Name -> Q [Dec]
deriveSqlKey tableName keyName = do
  conName <- getConName keyName

  [d|
    instance SqlKey $(conT tableName) $(conT keyName) where
      sqlKey2ID = $(pure $ ConE conName) . fromSqlKey
      id2sqlKey $(conP conName [varP (mkName "id'")]) = toSqlKey id'
    |]

-- | Attempts to map relevant SqlErrors to more processable types.
data DBError
  = SomeDBError        Text -- ^ Catch-all for irrelevant/unkown SqlErrors
  | UniquenessError    Text -- ^ Unique constraint violation err, holds name of violated column
  | AuthorizationError Text -- ^ See 'authorizationSqlError'
  | NotFoundError           -- ^ See 'resourceNotFoundSqlError'
  deriving (Show, Eq, Read)

-- | Maps both sides of a potentially errored SQL query.
-- 
-- > newtype Name = Name Text
-- >
-- > mkNames :: [Value Text] -> [Name]
-- > mkNames = map (\(Value name) -> Name name)
-- >
-- > result :: (Monad m, MonadDB m, MonadUnliftIO m) => m (Either ??? [Name])
-- > result = mapDBResult mkNames <$> runDBError do
-- >   select $ do
-- >     u <- from table @User
-- >     pure u.name
mapDBResult :: (FeatureError e) => (a -> b) -> Either DBError a -> Either e b
mapDBResult = bimap handleDBError

-- | Maps both sides of a potentially errored, potentially not-found SQL query.
mapMaybeDBResult :: (FeatureError e) => e -> (a -> b) -> Either DBError (Maybe a) -> Either e b
mapMaybeDBResult err f dbResult = do
  result <- handleDBError `first` dbResult
  f <$> maybeToRight err result

-- | Maps the error of a potentially errored SQL query/stmt.
mapDBError :: (FeatureError e) => Either DBError a -> Either e a
mapDBError = first handleDBError

-- | Maps the error of a potentially errored SQL query/stmt & ensures that the result is > 0.
--   Intended for use with something like Esqueleto's @insertCount@ or @deleteCount@.
expectDBNonZero :: (FeatureError e, Num cnt, Ord cnt) => e -> Either DBError cnt -> Either e ()
expectDBNonZero err dbResult = do
  result <- handleDBError `first` dbResult
  when (result == 0) $
    Left err

-- | A custom SqlError w/ code 45401
authorizationSqlError :: (Show e) => e -> SqlError
authorizationSqlError err = defaultSqlErr
  { sqlState = "45401"
  , sqlErrorMsg = "Authorization error"
  , sqlErrorDetail = show err
  }

-- | A custom SqlError w/ code 45404
resourceNotFoundSqlError :: SqlError
resourceNotFoundSqlError = defaultSqlErr
  { sqlState = "45404"
  , sqlErrorMsg = "Resource not found"
  }

-- | (Internal) Catches & maps any SqlErrors to 'DBError's
catchSqlError :: (MonadUnliftIO m) => m a -> m (Either DBError a)
catchSqlError stmt = catch @_ @SqlError
  (Right <$> stmt)
  (pure . Left . mapSqlError)

mapSqlError :: SqlError -> DBError
mapSqlError err
  | err.sqlState == "23505" = UniquenessError $ extractUniquenessViolation err
  | err.sqlState == "45401" = AuthorizationError $ decodeUtf8 err.sqlErrorDetail
  | err.sqlState == "45404" = NotFoundError
  | otherwise = SomeDBError $ show err

-- SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"unique_username\"", sqlErrorDetail = "Key (username)=(username) already exists.", sqlErrorHint = ""}

-- | (Internal) Extracts the name of the column whose uniqueness was violated
extractUniquenessViolation :: SqlError -> Text
extractUniquenessViolation = toText . extractViolatedColName . decodeUtf8 . sqlErrorDetail
  where extractViolatedColName = extractKeyField .- fromMaybe (error "")

extractKeyField :: String -> Maybe String
extractKeyField str = do
  rest <- stripPrefix "Key (" str
  let (keyField, _) = break (== ')') rest
  Just keyField

-- | (Internal) SqlError with irrelevant/unused fields pre-filled out
defaultSqlErr :: SqlError
defaultSqlErr = SqlError
  { sqlState = error "fill out sqlState"
  , sqlExecStatus = FatalError
  , sqlErrorMsg = error "fill out sqlErrorMsg"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

-- | (Internal) Gets the constructor name of some newtype
getConName :: Name -> Q Name
getConName typeName = do
  (TyConI tyCon) <- reify typeName
  
  case tyCon of
    NewtypeD _ _ _ _ (RecC    name _) _ -> pure name
    NewtypeD _ _ _ _ (NormalC name _) _ -> pure name
    NewtypeD {} -> fail "Newtype constructor not in expected format"
    _ -> fail "Expected a newtype"
