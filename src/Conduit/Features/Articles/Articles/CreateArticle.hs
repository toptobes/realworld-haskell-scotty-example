{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.CreateArticle where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBResult)
import Conduit.DB.Types (MonadDB, SqlKey (id2sqlKey, sqlKey2ID), runDB)
import Conduit.DB.Utils (zeroTime)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.DB (UserId)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Article(..))
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (mkNoIDSlug, mkSlug)
import Conduit.Features.Articles.Types (ArticleID, NoIDSlug(..), OneArticle, inArticleObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Utils ((>->))
import Conduit.Validation (Validations, are, fromJsonObj, notBlank)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (insert)
import Network.HTTP.Types (status201)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post, status)

data CreateArticleAction = CreateArticleAction
  { title       :: Text
  , description :: Text
  , body        :: Text
  , tagList     :: Maybe [Text] 
  } deriving (Generic)
    deriving anyclass (FromJSON)

validations :: Validations CreateArticleAction
validations CreateArticleAction {..} =
  [ (title, "title")      
  , (description, "description")
  , (body, "body")
  ] `are` notBlank

handleArticleCreation :: ScottyT AppM ()
handleArticleCreation = post "/api/articles" $ withAuth \user -> do
  action <- fromJsonObj validations

  article <- liftApp (createArticle action user.authedUserID)

  withFeatureErrorsHandled article $
    json . inArticleObj >->
    status status201

createArticle :: (CreateArticle m, AquireArticle m, AcquireProfile m) => CreateArticleAction -> UserID -> m (Either ArticleError OneArticle)
createArticle CreateArticleAction {..} author = runExceptT do
  let slug = mkNoIDSlug title
      article = ArticleInfo author slug title description body tagList
  
  articleID <- ExceptT $ insertArticle article
  ExceptT $ getArticle (mkSlug articleID slug) (Just author)

class (Monad m) => CreateArticle m where
  insertArticle :: ArticleInfo -> m (Either ArticleError ArticleID)

data ArticleInfo = ArticleInfo
  { author :: UserID
  , slug   :: NoIDSlug
  , title  :: Text
  , desc   :: Text
  , body   :: Text
  , tags   :: Maybe [Text]
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateArticle m where
  insertArticle :: ArticleInfo -> m (Either ArticleError ArticleID)
  insertArticle ArticleInfo {..} = mapDBResult sqlKey2ID <$> runDB do
    insert (mkArticle (id2sqlKey author) slug.unSlug title desc body (tags ?: []))

mkArticle :: UserId -> Text -> Text -> Text -> Text -> [Text] -> Article
mkArticle author slug title desc body tags = Article author slug title desc body tags zeroTime zeroTime
