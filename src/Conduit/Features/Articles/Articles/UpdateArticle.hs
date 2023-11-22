{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.UpdateArticle where

import Prelude hiding (put)
import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB (..), expectDBNonZero)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Article, assumingUserIsOwner)
import Conduit.Features.Articles.Errors (ArticleError (..))
import Conduit.Features.Articles.Slugs (extractIDFromSlug, mkNoIDSlug, mkSlug)
import Conduit.Features.Articles.Types (ArticleID(..), OneArticle, Slug(..), inArticleObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Val (NotBlank(..), fromJsonObj, (<?!<))
import Data.Aeson (FromJSON(..), (.:?), withObject)
import Database.Esqueleto.Experimental (set, updateCount, val, valkey, where_, (=.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, put)

data UpdateArticleAction = UpdateArticleAction
  { title       :: Maybe Text
  , description :: Maybe Text
  , body        :: Maybe Text
  }

instance FromJSON UpdateArticleAction where
  parseJSON = withObject "UpdateArticleAction" $ \v -> UpdateArticleAction
    <$> v .:? "title"       <?!< NotBlank
    <*> v .:? "description" <?!< NotBlank
    <*> v .:? "body"        <?!< NotBlank

handleArticleUpdate :: ScottyT AppM ()
handleArticleUpdate = put "/api/articles/:slug" $ withAuth \user -> do
  action <- fromJsonObj
  slug <- captureParam "slug" <&> Slug
  article <- runService $ updateArticle action slug user.authedUserID
  json $ inArticleObj article

updateArticle :: (UpdateArticle m, AquireArticle m, AcquireProfile m) => UpdateArticleAction -> Slug -> UserID -> m (Either ArticleError OneArticle)
updateArticle UpdateArticleAction {..} slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug

  let maybeNewSlug = title <&> mkSlug articleID . mkNoIDSlug
      toUpdate = ToUpdate title description body maybeNewSlug

  ExceptT $ updateArticleByID articleID userID toUpdate
  ExceptT $ getArticle (maybeNewSlug ?: slug) (Just userID)

class (Monad m) => UpdateArticle m where
  updateArticleByID :: ArticleID -> UserID -> ToUpdate -> m (Either ArticleError ())

data ToUpdate = ToUpdate
  { title :: Maybe Text
  , desc  :: Maybe Text
  , body  :: Maybe Text
  , slug  :: Maybe Slug
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => UpdateArticle m where
  updateArticleByID :: ArticleID -> UserID -> ToUpdate -> m (Either ArticleError ())
  updateArticleByID articleID userID ToUpdate {..} = expectDBNonZero ResourceNotFoundEx <$> runDB do
    assumingUserIsOwner ("todo" :: Text) userID articleID do
      updateCount @_ @Article $ \a -> do
        whenJust title \new -> set a [ #title =. val new        ]
        whenJust desc  \new -> set a [ #desc  =. val new        ]
        whenJust body  \new -> set a [ #body  =. val new        ]
        whenJust slug  \new -> set a [ #slug  =. val new.unSlug ]
        where_ (a.id ==. valkey articleID.unID)
