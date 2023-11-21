module Conduit.Features.Articles.Errors where

import Conduit.DB.Errors (DBError(..), FeatureErrorHandler(..), FeatureErrorMapper(..))
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Errors qualified as Account
import Conduit.Utils ((-.))
import Conduit.Validation (inErrMsgObj, mkErrObj)
import Network.HTTP.Types (status403, status404, status500)
import Network.HTTP.Types.Status (status422)
import Web.Scotty.Trans (ActionT, json, status)

data ArticleError
  = UserNotFoundEx
  | ResourceNotFoundEx -- General exception for simplicity's sake since tests don't need specific 404 error msgs
  | UserUnauthorizedEx
  | IllegalArticleDelEx
  | IllegalCommentDelEx
  | UniquenessEx Text
  | InvalidSlugEx
  | SomeDBEx DBError
  deriving (Show, Eq, Read)

instance FeatureErrorHandler ArticleError where
  withFeatureErrorsHandled :: (MonadIO m) => Either ArticleError a -> (a -> ActionT m ()) -> ActionT m ()
  withFeatureErrorsHandled = withFeatureErrorsHandled'

  handleDBError :: DBError -> ArticleError
  handleDBError = handleDBErr'

withFeatureErrorsHandled' :: (MonadIO m) => Either ArticleError a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled' (Left UserNotFoundEx)      _ = status status404
withFeatureErrorsHandled' (Left ResourceNotFoundEx)  _ = status status404
withFeatureErrorsHandled' (Left InvalidSlugEx)       _ = status status404
withFeatureErrorsHandled' (Left UserUnauthorizedEx)  _ = status status403
withFeatureErrorsHandled' (Left IllegalArticleDelEx) _ = status status403 >> json (inErrMsgObj @Text "You are not authorized to delete this article")
withFeatureErrorsHandled' (Left IllegalCommentDelEx) _ = status status403 >> json (inErrMsgObj @Text "You are not authorized to delete this comment")
withFeatureErrorsHandled' (Left (UniquenessEx e))    _ = status status422 >> json (mkErrObj [(e, "must be unique")])
withFeatureErrorsHandled' (Left (SomeDBEx e))        _ = print e >> status status500
withFeatureErrorsHandled' (Right a) action = action a

handleDBErr' :: DBError -> ArticleError
handleDBErr' (AuthorizationError e) = e & toString -. readMaybe -. fromMaybe (error $ "invalid authorization error: " <> show e)
handleDBErr' NotFoundError = ResourceNotFoundEx
handleDBErr' (UniquenessError e) = UniquenessEx e
handleDBErr' err = SomeDBEx err

instance FeatureErrorMapper AccountError ArticleError where
  mapFeatureError :: AccountError -> ArticleError
  mapFeatureError = accountErr2articleErr
  
accountErr2articleErr :: AccountError -> ArticleError
accountErr2articleErr Account.UserNotFoundEx = UserNotFoundEx
accountErr2articleErr (Account.SomeDBEx err) = SomeDBEx err
accountErr2articleErr _ = error "shouldn't need other maps; just fail-fast until I add proper logging lol"
