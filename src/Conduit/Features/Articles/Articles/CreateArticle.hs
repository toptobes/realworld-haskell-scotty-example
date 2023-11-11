{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.CreateArticle where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBResult)
import Conduit.DB.Types (MonadDB, SqlKey (id2sqlKey, sqlKey2ID), runDB)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (mkArticle)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (mkNoIDSlug, mkSlug)
import Conduit.Features.Articles.Types (ArticleID, NoIDSlug(..), OneArticle, inArticleObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Utils (InObj(..))
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (insert)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)

data CreateArticleAction = CreateArticleAction
  { title       :: Text
  , description :: Text
  , body        :: Text
  , tagList     :: Maybe [Text] 
  } deriving (Generic, FromJSON)

handleArticleCreation :: ScottyT AppM ()
handleArticleCreation = post "/api/articles" $ withAuth \user -> do
  (InObj _ action) <- jsonData
  user' <- liftApp (createArticle action user.authedUserID)
  withFeatureErrorsHandled user' $
    json . inArticleObj

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
