{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.DeleteArticle where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBError)
import Conduit.DB.Types (MonadDB, runDB)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.DB (Article, assumingUserOwnsArticle)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID(..), Slug(..))
import Conduit.Identity.Auth (authedUserID, withAuth)
import Database.Esqueleto.Experimental (delete, from, table, valkey, where_, (==.))
import Network.HTTP.Types (status200)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, status)
import Web.Scotty.Trans qualified as Scotty

handleArticleDelete :: ScottyT AppM ()
handleArticleDelete = Scotty.delete "/api/articles/:slug" $ withAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  result <- liftApp (deleteArticle slug user.authedUserID)
  withFeatureErrorsHandled result $ \_ ->
    status status200

deleteArticle :: (DeleteArticle m) => Slug -> UserID -> m (Either ArticleError ())
deleteArticle slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  ExceptT $ deleteArticleByID articleID userID

class (Monad m) => DeleteArticle m where
  deleteArticleByID :: ArticleID -> UserID -> m (Either ArticleError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => DeleteArticle m where
  deleteArticleByID :: ArticleID -> UserID -> m (Either ArticleError ())
  deleteArticleByID articleID userID = mapDBError <$> runDB do
    assumingUserOwnsArticle userID articleID do
      delete $ do
        a <- from (table @Article)
        where_ (a.id ==. valkey articleID.unID)
