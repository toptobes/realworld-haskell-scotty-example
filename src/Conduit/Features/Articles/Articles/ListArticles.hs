{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.ListArticles where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..))
import Conduit.Features.Account.Common.QueryAssociatedUser (queryAssociatedUser)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Common.QueryFavStats (queryFavStats)
import Conduit.Features.Articles.DB (Favorite, mkManyArticles)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (ManyArticles(..))
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Conduit.Utils ((-.))
import Data.List (lookup)
import Data.Text.Lazy.Builder qualified as TB
import Database.Esqueleto.Experimental (exists, from, groupBy, in_, just, leftJoin, limit, offset, on, orderBy, select, subSelectList, table, val, valList, where_, (:&) (..), (==.))
import Database.Esqueleto.Experimental qualified as E
import Database.Esqueleto.Internal.Internal (unsafeSqlValue)
import Relude.Extra (bimapBoth)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, ScottyT, captureParams, get, json)

data FilterOps = FilterOps
  { filterTag    :: Maybe  Text
  , filterAuthor :: Maybe [Text]
  , filterFavBy  :: Maybe  Text
  , filterLimit  :: Int64
  , filterOffset :: Int64
  }

handleListArticles :: ScottyT AppM ()
handleListArticles = get "/api/articles/" $ maybeWithAuth \user -> do
  filterOps <- parseFilterOps
  articles <- liftApp $ findArticles (user <&> authedUserID) filterOps
  withFeatureErrorsHandled articles json

parseFilterOps :: ActionT AppM FilterOps
parseFilterOps = do
  params <- captureParams <&> map (bimapBoth toStrict)

  pure $ FilterOps
    { filterTag    =  lookup "tag"       params
    , filterAuthor =  lookup "author"    params <&> (:[])
    , filterFavBy  =  lookup "favorited" params
    , filterLimit  = (lookup "limit"     params >>= toString -. readMaybe) ?: 20
    , filterOffset = (lookup "offset"    params >>= toString -. readMaybe) ?: 0
    }

class (Monad m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)
  findArticles userID FilterOps {..} = mapDBResult mkManyArticles <$> runDB do
    select $ do
      (a :& u, follows) <- queryAssociatedUser userID \a u -> 
        a.author ==. u.id

      groupBy (u.id, a.id)

      whenJust filterAuthor $ \names -> where_ $ u.username `in_` valList names

      whenJust filterFavBy $ \name -> where_ $
        exists $ do
          (_ :& u') <- from $
            table @Favorite
              `leftJoin`
            table @User
              `on` \(f :& u') ->
                just f.user ==. u'.id

          where_ $ u'.username ==. just (val name)

      whenJust filterFavBy $ \name -> where_ $
        just (val name) `in_` subSelectList do
          (_ :& u') <- from $
            table @Favorite
              `leftJoin`
            table @User
              `on` \(f :& u') ->
                just f.user ==. u'.id

          pure u'.username
          
      -- temp until I figure out how to get esqueleto to work with goddamn arrays
      whenJust filterTag $ \tag' -> where_ $ unsafeSqlValue ("'s" <> TB.fromText tag' <> "' = ANY(array(select json_array_elements_text(tags::json)))")

      limit  filterLimit
      offset filterOffset

      let (favorited, numFavs) = queryFavStats userID a

      orderBy [E.desc a.created]

      pure (a, u, follows, favorited, numFavs)
