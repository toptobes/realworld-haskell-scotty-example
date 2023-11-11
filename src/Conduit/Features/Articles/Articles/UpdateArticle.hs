{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.UpdateArticle where

import Prelude hiding (put)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBError)
import Conduit.DB.Types (MonadDB, runDB)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Article, userOwnsArticle, assumingUserOwnsArticle)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (extractIDFromSlug, mkNoIDSlug, mkSlug)
import Conduit.Features.Articles.Types (ArticleID(..), OneArticle, Slug(..), inArticleObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Utils (InObj(..))
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (set, update, val, valkey, where_, (=.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, jsonData, put)

data UpdateArticleAction = UpdateArticleAction
  { title       :: Maybe Text
  , description :: Maybe Text
  , body        :: Maybe Text
  } deriving (Generic, FromJSON)

handleArticleUpdate :: ScottyT AppM ()
handleArticleUpdate = put "/api/articles/:slug" $ withAuth \user -> do
  (InObj _ action) <- jsonData
  slug <- captureParam "slug" <&> Slug
  user' <- liftApp (updateArticle action slug user.authedUserID)
  withFeatureErrorsHandled user' $
    json . inArticleObj

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
  updateArticleByID articleID userID ToUpdate {..} = mapDBError <$> runDB do
    assumingUserOwnsArticle userID articleID do
      update @_ @Article $ \a -> do
        whenJust title \new -> set a [ #title =. val new        ]
        whenJust desc  \new -> set a [ #desc  =. val new        ]
        whenJust body  \new -> set a [ #body  =. val new        ]
        whenJust slug  \new -> set a [ #slug  =. val new.unSlug ]
        where_ (a.id ==. valkey articleID.unID)
