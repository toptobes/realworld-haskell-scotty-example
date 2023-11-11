{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.GetArticle where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), SqlKey (id2sqlKey, sqlKey2ID))
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile, findUserProfileByID)
import Conduit.Features.Account.Types (UserID, UserProfile)
import Conduit.Features.Articles.DB (Article(..), Favorite)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (ArticleID, OneArticle(..), Slug(..), inArticleObj)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), Value(..), count, from, groupBy, just, leftJoin, on, selectOne, table, val, where_, (&&.), (:&)(..), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)

handleGetArticle :: ScottyT AppM ()
handleGetArticle = get "/api/articles/:slug" $ maybeWithAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  article <- liftApp $ getArticle slug (user <&> authedUserID)
  withFeatureErrorsHandled article $
    json . inArticleObj

getArticle :: (AquireArticle m, AcquireProfile m) => Slug -> Maybe UserID -> m (Either ArticleError OneArticle)
getArticle slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug

  (authorID, mkArticle) <- ExceptT $ findArticleByID articleID userID

  profile <- ExceptT $ findUserProfileByID authorID userID
  pure $ mkArticle profile

type MkOneArticle = UserProfile -> OneArticle

class (Monad m) => AquireArticle m where
  findArticleByID :: ArticleID -> Maybe UserID -> m (Either ArticleError (UserID, MkOneArticle))

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticle m where
  findArticleByID :: ArticleID -> Maybe UserID -> m (Either ArticleError (UserID, MkOneArticle))
  findArticleByID articleID userID = mapMaybeDBResult ArticleNotFoundEx mkArticleInfo <$> runDB do
    selectOne $ do
      (a :& f) <- from $
        table @Article
          `leftJoin`
        table @Favorite
          `on` \(a :& f) ->
            (just a.id ==. f.article) &&. (f.user ==. val (userID <&> id2sqlKey))

      where_ (a.id ==. val (id2sqlKey articleID))

      groupBy (a.id, f.user, f.article)

      pure (a, f, count f.article)

mkArticleInfo :: (Entity Article, Maybe a, Value Int) -> (UserID, MkOneArticle)
mkArticleInfo (Entity _ Article {..}, isJust -> favorited, Value numFavs) =
  ( sqlKey2ID articleAuthor
  , \profile -> OneArticle
      { numFavs = numFavs,     slug      = Slug articleSlug
      , body    = articleBody, title     = articleTitle
      , desc    = articleDesc, favorited = favorited
      , tags    = articleTags, created   = articleCreated
      , author  = profile,     updated   = articleUpdated
      }
  )
