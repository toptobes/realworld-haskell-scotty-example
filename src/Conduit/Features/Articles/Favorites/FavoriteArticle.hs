{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Favorites.FavoriteArticle where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), id2sqlKey, mapDBError)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Favorite(Favorite))
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID, OneArticle, Slug(..), inArticleObj)
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (insert_)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, post)

handleArticleFavorite :: ScottyT AppM ()
handleArticleFavorite = post "/api/articles/:slug/favorite" $ withAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  article <- runService $ favoriteArticle slug user.authedUserID
  json $ inArticleObj article

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
