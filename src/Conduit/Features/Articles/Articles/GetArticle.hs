{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.GetArticle where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..), SqlKey (id2sqlKey, sqlKey2ID))
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile, findUserProfileByID)
import Conduit.Features.Account.Types (UserID, UserProfile)
import Conduit.Features.Articles.DB (Article(..), Favorite)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID, OneArticle(..), Slug(..), inArticleObj)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), Value(..), exists, from, just, selectOne, subSelectCount, table, val, where_, (&&.), (==.))
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
      a <- from $ table @Article

      where_ (a.id ==. val (id2sqlKey articleID))

      let favorited = exists $ void $ from (table @Favorite) 
            `suchThat` \f' -> 
              (a.id ==. f'.article) &&. (just f'.user ==. val (userID <&> id2sqlKey))

      let numFavs = subSelectCount $ from (table @Favorite)
            `suchThat` \f' -> 
              a.id ==. f'.article

      pure (a, favorited, numFavs)

mkArticleInfo :: (Entity Article, Value Bool, Value Int) -> (UserID, MkOneArticle)
mkArticleInfo (Entity _ Article {..}, Value favorited, Value numFavs) =
  ( sqlKey2ID articleAuthor
  , \profile -> OneArticle
      { numFavs = numFavs,     slug      = Slug articleSlug
      , body    = articleBody, title     = articleTitle
      , desc    = articleDesc, favorited = favorited
      , tags    = articleTags, created   = articleCreated
      , author  = profile,     updated   = articleUpdated
      }
  )
