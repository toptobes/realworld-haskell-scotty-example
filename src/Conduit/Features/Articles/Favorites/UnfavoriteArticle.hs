{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Favorites.UnfavoriteArticle where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), mapDBError)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.Articles.GetArticle (AquireArticle, getArticle)
import Conduit.Features.Articles.DB (Favorite)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID(..), OneArticle, Slug(..), inArticleObj)
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (delete, from, table, valkey, where_, (&&.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json)
import Web.Scotty.Trans qualified as Scotty

handleArticleUnfavorite :: ScottyT AppM ()
handleArticleUnfavorite = Scotty.delete "/api/articles/:slug/favorite" $ withAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  article <- runService $ unfavoriteArticle slug user.authedUserID
  json $ inArticleObj article

unfavoriteArticle :: (DeleteFavorite m, AquireArticle m, AcquireProfile m) => Slug -> UserID -> m (Either ArticleError OneArticle)
unfavoriteArticle slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  ExceptT $ deleteFavorite articleID userID
  ExceptT $ getArticle slug (Just userID)

class (Monad m) => DeleteFavorite m where
  deleteFavorite :: ArticleID -> UserID -> m (Either ArticleError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => DeleteFavorite m where
  deleteFavorite :: ArticleID -> UserID -> m (Either ArticleError ())
  deleteFavorite articleID userID = mapDBError <$> runDB do
    delete $ do
      f <- from $ table @Favorite
      where_ $ (f.article ==. valkey articleID.unID) &&. (f.user ==. valkey userID.unID)
