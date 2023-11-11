{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Favorites.FavoriteArticle where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapDBError, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..), id2sqlKey)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Favorite(Favorite))
import Conduit.Features.Articles.Errors
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (insert_)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, post)

handleArticleFavorite :: ScottyT AppM ()
handleArticleFavorite = post "/api/articles/:slug/favorite" $ withAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  article <- liftApp (favoriteArticle slug user.authedUserID)
  withFeatureErrorsHandled article $
    json . inArticleObj

favoriteArticle :: (CreateFavorite m, AquireArticle m, AcquireProfile m) => Slug -> UserID -> m (Either ArticleError OneArticle)
favoriteArticle slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  ExceptT $ addFavorite articleID userID
  ExceptT $ getArticle slug (Just userID)

class (Monad m) => CreateFavorite m where
  addFavorite :: ArticleID -> UserID -> m (Either ArticleError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateFavorite m where
  addFavorite :: ArticleID -> UserID -> m (Either ArticleError ())
  addFavorite articleID userID = mapDBError <$> runDB do 
    insert_ $ Favorite (id2sqlKey userID) (id2sqlKey articleID) 
