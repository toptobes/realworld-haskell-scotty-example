{-# LANGUAGE AllowAmbiguousTypes #-}

module Conduit.DB.Errors where

import Web.Scotty.Trans (ActionT)
import Database.PostgreSQL.Simple (SqlError (..), ExecStatus (FatalError))
import UnliftIO (MonadUnliftIO, catch)
import Conduit.Utils ((-.))
import Data.List (stripPrefix)

data DBError
  = SomeDBError SqlError
  | UniquenessError Text
  | AuthorizationError Text
  deriving (Show, Eq)

class FeatureErrorHandler e where
  withFeatureErrorsHandled :: (MonadIO m) => Either e a -> (a -> ActionT m ()) -> ActionT m ()
  handleDBError :: DBError -> e

mapDBResult :: (FeatureErrorHandler e) => (a -> b) -> Either DBError a -> Either e b
mapDBResult = bimap handleDBError

mapMaybeDBResult :: (FeatureErrorHandler e) => e -> (a -> b) -> Either DBError (Maybe a) -> Either e b
mapMaybeDBResult err f dbResult = do
  result <- handleDBError `first` dbResult
  f <$> maybeToRight err result

mapDBError :: (FeatureErrorHandler e) => Either DBError a -> Either e a
mapDBError = mapDBResult id

class FeatureErrorMapper e1 e2 where
  mapFeatureError :: e1 -> e2

catchSqlError :: (MonadUnliftIO m) => m a -> m (Either DBError a)
catchSqlError stmt = catch @_ @SqlError
  (Right <$> stmt)
  (pure . Left . mapSqlError)

mapSqlError :: SqlError -> DBError
mapSqlError err
  | err.sqlState == "23505" = UniquenessError $ extractUniquenessViolation err
  | err.sqlState == "45000" = AuthorizationError $ decodeUtf8 err.sqlErrorDetail
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

authorizationSqlError :: SqlError
authorizationSqlError = SqlError
  { sqlState = "45000"
  , sqlExecStatus = FatalError
  , sqlErrorMsg = "Authorization error"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }
