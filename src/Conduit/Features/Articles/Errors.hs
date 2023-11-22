module Conduit.Features.Articles.Errors where

import Conduit.DB.Core (DBError(..))
import Conduit.Errors (FeatureError(..), FeatureErrorMapper(..))
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Errors qualified as Account
import Conduit.Utils ((.-))
import Conduit.Val (ValErrs(..), inErrMsgObj)
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

instance FeatureError ArticleError where
  handleFeatureError = handleFeatureError'
  handleDBError = handleDBErr'

handleFeatureError' :: (MonadIO m) => ArticleError -> ActionT m ()
handleFeatureError' UserNotFoundEx      = status status404
handleFeatureError' ResourceNotFoundEx  = status status404
handleFeatureError' InvalidSlugEx       = status status404
handleFeatureError' UserUnauthorizedEx  = status status403
handleFeatureError' IllegalArticleDelEx = status status403 >> json (inErrMsgObj @Text "You are not authorized to delete this article")
handleFeatureError' IllegalCommentDelEx = status status403 >> json (inErrMsgObj @Text "You are not authorized to delete this comment")
handleFeatureError' (UniquenessEx e)    = status status422 >> json (ValErrs [(e, "must be unique")])
handleFeatureError' (SomeDBEx e)        = print e >> status status500

handleDBErr' :: DBError -> ArticleError
handleDBErr' (AuthorizationError e) = e & toString .- readMaybe .- fromMaybe (error $ "invalid authorization error: " <> show e)
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
