{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.FeedArticles where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBResult)
import Conduit.DB.Types (MonadDB, id2sqlKey, runDB)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.Common.FindFollowersByID (AquireFollowers, findFollowersByID)
import Conduit.Features.Account.Common.QueryAssociatedUser (queryAssociatedUser)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.DB (Favorite, mkManyArticles)
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Types (ManyArticles(..))
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Utils ((-.))
import Data.List (lookup)
import Database.Esqueleto.Experimental (exists, from, groupBy, in_, just, limit, offset, orderBy, select, subSelectCount, table, val, valList, valkey, where_, (&&.), (:&)(..), (==.))
import Database.Esqueleto.Experimental qualified as E
import Relude.Extra (bimapBoth)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, ScottyT, captureParams, get, json)

data FilterOps = FilterOps
  { filterLimit  :: Int64
  , filterOffset :: Int64
  }

handleFeedArticles :: ScottyT AppM ()
handleFeedArticles = get "/api/articles/feed" $ withAuth \user -> do
  filterOps <- parseFilterOps
  articles <- liftApp $ getFeedArticles user.authedUserID filterOps
  withFeatureErrorsHandled articles json

getFeedArticles :: (AquireArticles m, AquireFollowers m) => UserID -> FilterOps -> m (Either ArticleError ManyArticles)
getFeedArticles userID ops = runExceptT do
  followers <- ExceptT $ findFollowersByID userID
  ExceptT $ findArticles userID followers ops

parseFilterOps :: ActionT AppM FilterOps
parseFilterOps = do
  params <- captureParams <&> map (bimapBoth toStrict)

  pure $ FilterOps
    { filterLimit  = (lookup "limit"  params >>= toString -. readMaybe) ?: 20
    , filterOffset = (lookup "offset" params >>= toString -. readMaybe) ?: 0
    }

class (Monad m) => AquireArticles m where
  findArticles :: UserID -> [UserID] -> FilterOps -> m (Either ArticleError ManyArticles)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticles m where
  findArticles :: UserID -> [UserID] -> FilterOps -> m (Either ArticleError ManyArticles)
  findArticles userID (map id2sqlKey -> followeeIDs) FilterOps {..} = mapDBResult mkManyArticles <$> runDB do
    select $ do
      ~(a :& u, _) <- queryAssociatedUser Nothing $ \a u -> 
        a.author ==. u.id

      groupBy (u.id, a.id)

      where_ $ u.id `in_` valList followeeIDs
      
      limit  filterLimit
      offset filterOffset

      let favorited = exists $ void $ from (table @Favorite)
            `suchThat` \f' ->
              (a.id ==. f'.article) &&. (just f'.user ==. just (valkey userID.unID))

      let numFavs = subSelectCount @Int $ from (table @Favorite)
            `suchThat` \f' ->
              a.id ==. f'.article

      orderBy [E.desc a.created]

      pure (a, u, val True, favorited, numFavs)
