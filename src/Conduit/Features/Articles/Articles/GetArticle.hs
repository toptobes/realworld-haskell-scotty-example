{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.GetArticle where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..), id2sqlKey)
import Conduit.Features.Account.Common.FindProfileByID (AcquireProfile)
import Conduit.Features.Account.Common.QueryAssociatedUser (queryAssociatedUser)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Common.QueryFavStats (queryFavStats)
import Conduit.Features.Articles.DB (mkOneArticle)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID, OneArticle(..), Slug(..), inArticleObj)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (selectOne, val, where_, (:&)(..), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)

handleGetArticle :: ScottyT AppM ()
handleGetArticle = get "/api/articles/:slug" $ maybeWithAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  article <- liftApp $ getArticle slug (user <&> authedUserID)
  withFeatureErrorsHandled article $
    json . inArticleObj

getArticle :: (AquireArticle m, AcquireProfile m) => Slug -> Maybe UserID -> m (Either ArticleError OneArticle)
getArticle slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  ExceptT $ findArticleByID articleID userID

class (Monad m) => AquireArticle m where
  findArticleByID :: ArticleID -> Maybe UserID -> m (Either ArticleError OneArticle)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticle m where
  findArticleByID :: ArticleID -> Maybe UserID -> m (Either ArticleError OneArticle)
  findArticleByID articleID userID = mapMaybeDBResult ResourceNotFoundEx mkOneArticle <$> runDB do
    selectOne $ do
      (a :& u, follows) <- queryAssociatedUser userID \a u -> 
        a.author ==. u.id

      where_ (a.id ==. val (id2sqlKey articleID))

      let (favorited, numFavs) = queryFavStats userID a

      pure (a, u, follows, favorited, numFavs)
