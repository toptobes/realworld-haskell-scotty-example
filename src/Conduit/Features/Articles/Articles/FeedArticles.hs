{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.FeedArticles where

import Prelude hiding (get, on)
import Web.Scotty.Trans (ScottyT, ActionT, captureParams, json, get)
import Conduit.App.Monad (AppM, liftApp)
import Data.List (lookup)
import Conduit.Utils ((-.))
import Relude.Extra (bimapBoth)
import Conduit.Features.Articles.Types (ManyArticles(..), OneArticle (..), Slug (..))
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBResult)
import Conduit.Features.Account.Types (UserID(..), UserProfile (..))
import Conduit.Identity.Auth (withAuth, authedUserID)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.DB (Article (..), Favorite)
import Database.Esqueleto.Experimental (Entity(..), Value(..), type (:&) (..), select, from, leftJoin, table, on, just, (==.), groupBy, limit, offset, exists, (&&.), val, subSelectCount, orderBy, valkey)
import Database.Esqueleto.Experimental qualified as E
import Conduit.Features.Account.DB (User (..))
import Conduit.DB.Types (MonadDB, runDB, SqlKey (id2sqlKey))
import UnliftIO (MonadUnliftIO)
import Conduit.Features.Account.Exports.FindFollowersByID (AquireFollowers)
import Conduit.DB.Utils (suchThat)

data FilterOps = FilterOps
  { filterLimit  :: Int64
  , filterOffset :: Int64
  }

handleFeedArticles :: ScottyT AppM ()
handleFeedArticles = get "/api/articles/" $ withAuth \user -> do
  filterOps <- parseFilterOps
  article <- liftApp $ getFeedArticles user.authedUserID filterOps
  withFeatureErrorsHandled article json

getFeedArticles :: (AquireArticles m, AquireFollowers m) => UserID -> FilterOps -> m (Either ArticleError ManyArticles)
getFeedArticles = undefined

parseFilterOps :: ActionT AppM FilterOps
parseFilterOps = do
  params <- captureParams <&> map (bimapBoth toStrict)

  pure $ FilterOps
    { filterLimit  = (lookup "limit"     params >>= toString -. readMaybe) ?: 20
    , filterOffset = (lookup "offset"    params >>= toString -. readMaybe) ?: 0
    }

class (Monad m) => AquireArticles m where
  findArticles :: UserID -> [UserID] -> FilterOps -> m (Either ArticleError ManyArticles)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticles m where
  findArticles :: UserID -> [UserID] -> FilterOps -> m (Either ArticleError ManyArticles)
  findArticles userID followeeIDs FilterOps {..} = mapDBResult toManyArticles <$> runDB do
    let followees = map unID followeeIDs

    select $ do
      (a :& u) <- from $
        table @Article
          `leftJoin`
        table @User
          `on` \(a :& u) ->
            just a.author ==. u.id

      groupBy (u.id, a.id)
      
      limit  filterLimit
      offset filterOffset

      let favorited = exists $ void $ from (table @Favorite)
            `suchThat` \f' ->
              (a.id ==. f'.article) &&. (just f'.user ==. just (valkey userID.unID))

      let numFavs = subSelectCount @Int $ from (table @Favorite)
            `suchThat` \f' ->
              a.id ==. f'.article

      orderBy [E.desc a.created]

      pure (a, u, favorited, numFavs)

toManyArticles :: [(Entity Article, Maybe (Entity User), Value Bool, Value Int)] -> ManyArticles
toManyArticles = ManyArticles . map toOneArticle

toOneArticle :: (Entity Article, Maybe (Entity User), Value Bool, Value Int) -> OneArticle
toOneArticle (_, Nothing, _, _) = error "I'll deal with this later..."
toOneArticle (Entity _ Article {..}, Just (Entity _ User {..}), Value faved, Value numFavs) = OneArticle
  { slug = Slug articleSlug
  , title = articleTitle
  , tags = reverse articleTags
  , body = articleBody
  , created = articleCreated
  , updated = articleUpdated
  , favorited = faved
  , numFavs = numFavs
  , desc = articleDesc
  , author = UserProfile
    { userName = userUsername
    , userImage = userImage
    , userBio = userBio
    , userFollowed = True
    }
  }
