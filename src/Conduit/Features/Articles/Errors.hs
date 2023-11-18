module Conduit.Features.Articles.Errors where


import Conduit.DB.Errors (DBError(..), FeatureErrorHandler(..), FeatureErrorMapper(..))
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Errors qualified as Account
import Network.HTTP.Types (status404, status500, status403)
import Web.Scotty.Trans (ActionT, status)

data ArticleError
  = UserNotFoundEx
  | ArticleNotFoundEx
  | CommentNotFoundEx
  | UserUnauthorizedEx
  | InvalidSlugEx
  | SomeDBEx DBError
  deriving (Show, Eq)

instance FeatureErrorHandler ArticleError where
  withFeatureErrorsHandled :: (MonadIO m) => Either ArticleError a -> (a -> ActionT m ()) -> ActionT m ()
  withFeatureErrorsHandled = withFeatureErrorsHandled'

  handleDBError :: DBError -> ArticleError
  handleDBError = handleDBErr'

withFeatureErrorsHandled' :: (MonadIO m) => Either ArticleError a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled' (Left UserNotFoundEx)     _ = status status404
withFeatureErrorsHandled' (Left ArticleNotFoundEx)  _ = status status404
withFeatureErrorsHandled' (Left CommentNotFoundEx)  _ = print @Text "Comment not found??" >> status status500
withFeatureErrorsHandled' (Left InvalidSlugEx)      _ = status status404
withFeatureErrorsHandled' (Left UserUnauthorizedEx) _ = status status403
withFeatureErrorsHandled' (Left (SomeDBEx e))       _ = print e >> status status500
withFeatureErrorsHandled' (Right a) action = action a

handleDBErr' :: DBError -> ArticleError
handleDBErr' (AuthorizationError _) = UserUnauthorizedEx
handleDBErr' err = SomeDBEx err

instance FeatureErrorMapper AccountError ArticleError where
  mapFeatureError :: AccountError -> ArticleError
  mapFeatureError = accountErr2articleErr
  
accountErr2articleErr :: AccountError -> ArticleError
accountErr2articleErr Account.UserNotFoundEx = UserNotFoundEx
accountErr2articleErr (Account.SomeDBEx err) = SomeDBEx err
accountErr2articleErr _ = error "shouldn't need other maps; just fail-fast until I add proper logging lol"
