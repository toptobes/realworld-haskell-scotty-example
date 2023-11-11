{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.ListArticles where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), SqlKey (id2sqlKey, sqlKey2ID))
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled, mapDBResult)
import Conduit.Features.Account.Exports.FindProfileByID (AcquireProfile, findUserProfileByID)
import Conduit.Features.Account.Types (UserID, UserProfile)
import Conduit.Features.Articles.DB (Article(..), Favorite)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (ArticleID, OneArticle(..), Slug(..), inArticleObj, ManyArticles)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), Value(..), count, from, groupBy, just, leftJoin, on, selectOne, table, val, where_, (&&.), (:&)(..), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json, ActionT, captureParams, Param)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Data.List (lookup)
import Relude.Extra (bimapBoth)
import Conduit.Utils ((-.))

data FilterOps = FilterOps
  { tag    :: Maybe Text
  , author :: Maybe Text
  , favBy  :: Maybe Text
  , limit  :: Int
  , offset :: Int
  }

handleGetArticles :: ScottyT AppM ()
handleGetArticles = get "/api/articles/" $ maybeWithAuth \user -> do
  filterOps <- parseFilterOps
  article <- liftApp $ findArticles (user <&> authedUserID) filterOps
  withFeatureErrorsHandled article $
    json . inArticleObj

parseFilterOps :: ActionT AppM FilterOps
parseFilterOps = do
  params <- captureParams <&> map (bimapBoth toStrict)

  pure $ FilterOps
    { tag    =  lookup "tag"       params
    , author =  lookup "author"    params
    , favBy  =  lookup "favorited" params
    , limit  = (lookup "limit"     params >>= toString -. readMaybe) ?: 20
    , offset = (lookup "offset"    params >>= toString -. readMaybe) ?: 0
    }

class (Monad m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)
  findArticles userID FilterOps {..} = mapDBResult undefined <$> runDB do
    undefined
  