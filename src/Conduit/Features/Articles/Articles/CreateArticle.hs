{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.CreateArticle where
  
import Conduit.App.Monad (AppM, liftApp)
import Conduit.Utils (InObj(..))
import Web.Scotty.Trans (ScottyT, post, jsonData, json)
import Conduit.Errors (FeatureErrorHandler(..), mapDBResult, mapDBError)
import Conduit.Features.Articles.Types (inArticleObj, OneArticle)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Identity.Auth (withAuth, AuthedUser, authedUserID)
import Conduit.DB (MonadDB (runDB), zeroTime)
import UnliftIO (MonadUnliftIO)
import Data.Aeson (FromJSON)
import Conduit.Features.Articles.DB (Article(..), mkArticle)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Account.DB (userID2sqlKey)
import Database.Esqueleto.Experimental (insert, PersistStoreWrite (insert_))
import Conduit.Features.Account.User.GetProfile (getUserProfile, AcquireUser)

data CreateArticleAction = CreateArticleAction
  { title ::  Text
  , desc  ::  Text
  , body  ::  Text
  , tags  :: [Text] 
  } deriving (Generic, FromJSON)

handleArticleCreation :: ScottyT AppM ()
handleArticleCreation = post "/api/users" $ withAuth \user -> do
  (InObj _ action) <- jsonData
  user' <- liftApp (createArticle action user)
  withFeatureErrorsHandled user' $
    json . inArticleObj

createArticle :: (AcquireUser m, CreateArticle m) => CreateArticleAction -> AuthedUser -> m (Either ArticleError OneArticle)
createArticle CreateArticleAction {..} (authedUserID -> author) = runExceptT do
  let slug = ""
      article = ArticleInfo author slug title desc body tags
  
  -- profile <- ExceptT $ getUserProfile
  ExceptT $ insertArticle article
  undefined

class (Monad m) => CreateArticle m where
  insertArticle :: ArticleInfo -> m (Either ArticleError ())

data ArticleInfo = ArticleInfo
  { author :: UserID
  , slug   ::  Text
  , title  ::  Text
  , desc   ::  Text
  , body   ::  Text
  , tags   :: [Text] 
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateArticle m where
  insertArticle :: ArticleInfo -> m (Either ArticleError ())
  insertArticle ArticleInfo {..} = mapDBError <$> runDB do
    insert_ (mkArticle (userID2sqlKey author) "" title desc body tags)
