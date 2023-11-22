{-# LANGUAGE AllowAmbiguousTypes #-}

module Conduit.DB.Errors where

import Conduit.Utils ((-.))
import Data.List (stripPrefix)
import Database.PostgreSQL.Simple (ExecStatus(..), SqlError(..))
import UnliftIO (MonadUnliftIO, catch)
import Web.Scotty.Trans (ActionT)

-- | Attempts to map relevant SqlErrors to more processable types.
data DBError
  = SomeDBError        Text -- ^ Catch-all for irrelevant/unkown SqlErrors
  | UniquenessError    Text -- ^ Unique constraint violation err, holds name of violated column
  | AuthorizationError Text -- ^ See 'authorizationSqlError'
  | NotFoundError           -- ^ See 'resourceNotFoundSqlError'
  deriving (Show, Eq, Read)

-- | Way for features to map from DBErrors -> Feature errors -> application errors/responses.
--   See also 'withFeatureErrorsHandled'.
--   
--   P.S. I would put this in "Conduit.Validation" or its own file, but it'd be stuck in a cyclic dependency
--   so my hand is kinda forced here and I don't wanna deal with an hs-boot atm. It doesn't matter too much
--   here anyways so it's fine enough in this small project.
class FeatureError e where
  -- | Converts a feature error into some scotty response
  handleFeatureError :: (MonadIO m) => e -> ActionT m ()
  -- | Converts a DBError into some feature error
  handleDBError :: DBError -> e

-- | Converts a potentially failed service's result to either the appropriate error or successful request.
-- 
-- > data MyErr = Aw Text
-- > data MyResult = Yay Text deriving (Generic, ToJSON)
-- > 
-- > instance FeatureError MyErr where
-- >   handleFeatureError (ResultErr msg) = do 
-- >     status status500
-- >     text msg
-- > 
-- > endpoint = get "/" $ do
-- >   (result :: Either MyErr MyResult) <- someService
-- >   withFeatureErrorsHandled result $ \res ->
-- >     json res
withFeatureErrorsHandled :: (MonadIO m, FeatureError e) => Either e a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled (Left  e) _ = handleFeatureError e
withFeatureErrorsHandled (Right e) action = action e

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

-- | Provides an easy way for errors to translate between features to facilitate cross-feature code sharing
class FeatureErrorMapper e1 e2 where
  mapFeatureError :: e1 -> e2

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
  where extractViolatedColName = extractKeyField -. fromMaybe (error "")

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
