{-# LANGUAGE AllowAmbiguousTypes #-}

module Conduit.DB.Errors where

import Conduit.Utils ((-.))
import Data.List (stripPrefix)
import Database.PostgreSQL.Simple (ExecStatus(..), SqlError(..))
import UnliftIO (MonadUnliftIO, catch)
import Web.Scotty.Trans (ActionT)

data DBError
  = SomeDBError Text
  | UniquenessError Text
  | AuthorizationError Text
  | NotFoundError
  deriving (Show, Eq, Read)

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
mapDBError = first handleDBError

expectDBNonZero :: (FeatureErrorHandler e, Num cnt, Ord cnt) => e -> Either DBError cnt -> Either e ()
expectDBNonZero err dbResult = do
  result <- handleDBError `first` dbResult
  when (result == 0) $
    Left err

class FeatureErrorMapper e1 e2 where
  mapFeatureError :: e1 -> e2

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

extractUniquenessViolation :: SqlError -> Text
extractUniquenessViolation = toText . extractViolatedColName . decodeUtf8 . sqlErrorDetail
  where extractViolatedColName = extractKeyField -. fromMaybe (error "")

extractKeyField :: String -> Maybe String
extractKeyField str = do
  rest <- stripPrefix "Key (" str
  let (keyField, _) = break (== ')') rest
  Just keyField

defaultSqlErr :: SqlError
defaultSqlErr = SqlError
  { sqlState = error "fill out sqlState"
  , sqlExecStatus = FatalError
  , sqlErrorMsg = error "fill out sqlErrorMsg"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

authorizationSqlError :: (Show e) => e -> SqlError
authorizationSqlError err = defaultSqlErr
  { sqlState = "45401"
  , sqlErrorMsg = "Authorization error"
  , sqlErrorDetail = show err
  }

resourceNotFoundSqlError :: SqlError
resourceNotFoundSqlError = defaultSqlErr
  { sqlState = "45404"
  , sqlErrorMsg = "Resource not found"
  }
