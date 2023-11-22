{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.CreateArticle where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), SqlKey(..), mapDBResult)
import Conduit.DB.Utils (dbTimeNow)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.DB (UserId)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Article(..))
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (mkNoIDSlug, mkSlug)
import Conduit.Features.Articles.Types (ArticleID, NoIDSlug(..), OneArticle, inArticleObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Val (NotBlank(..), (<!<), fromJsonObj)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Database.Esqueleto.Experimental (insert)
import Network.HTTP.Types (status201)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post, status)

data CreateArticleAction = CreateArticleAction
  { title       :: Text
  , description :: Text
  , body        :: Text
  , tagList     :: Maybe [Text]
  }

instance FromJSON CreateArticleAction where
  parseJSON = withObject "CreateArticleAction" $ \v -> CreateArticleAction
    <$> v .: "title"       <!< NotBlank
    <*> v .: "description" <!< NotBlank
    <*> v .: "body"        <!< NotBlank
    <*> v .: "tagList"

handleArticleCreation :: ScottyT AppM ()
handleArticleCreation = post "/api/articles" $ withAuth \user -> do
  action <- fromJsonObj
  article <- runService $ createArticle action user.authedUserID
  status status201
  json $ inArticleObj article

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

instance (Monad m, Conduit.DB.Core.MonadDB m, MonadUnliftIO m) => CreateArticle m where
  insertArticle :: ArticleInfo -> m (Either ArticleError ArticleID)
  insertArticle ArticleInfo {..} = Conduit.DB.Core.mapDBResult Conduit.DB.Core.sqlKey2ID <$> Conduit.DB.Core.runDB do
    insert (mkArticle (Conduit.DB.Core.id2sqlKey author) slug.unSlug title desc body (tags ?: []))

mkArticle :: UserId -> Text -> Text -> Text -> Text -> [Text] -> Article
mkArticle author slug title desc body tags = Article author slug title desc body tags dbTimeNow dbTimeNow