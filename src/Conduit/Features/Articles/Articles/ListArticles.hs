{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Articles.ListArticles where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), SqlKey (id2sqlKey))
import Conduit.DB.Errors (withFeatureErrorsHandled, mapDBResult)
import Conduit.Features.Account.Types (UserID, UserProfile (..))
import Conduit.Features.Articles.DB (Article(..), Favorite)
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (ManyArticles(..), OneArticle(..), Slug (Slug))
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), Value(..), from, groupBy, just, leftJoin, on, table, val, where_, (&&.), (:&)(..), (==.), select, (?.), limit, offset, exists, subSelectCount, orderBy)
import Database.Esqueleto.Experimental qualified as E
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, get, json, ActionT, captureParams)
import Data.List (lookup)
import Relude.Extra (bimapBoth)
import Conduit.Utils ((-.))
import Conduit.Features.Account.DB (User (..), Follow)
import Conduit.DB.Utils (suchThat)
import Database.Esqueleto.Internal.Internal (unsafeSqlValue)
import Data.Text.Lazy.Builder qualified as TB

data FilterOps = FilterOps
  { filterTag    :: Maybe Text
  , filterAuthor :: Maybe Text
  , filterFavBy  :: Maybe Text
  , filterLimit  :: Int64
  , filterOffset :: Int64
  }

handleGetArticles :: ScottyT AppM ()
handleGetArticles = get "/api/articles/" $ maybeWithAuth \user -> do
  filterOps <- parseFilterOps
  article <- liftApp $ findArticles (user <&> authedUserID) filterOps
  withFeatureErrorsHandled article json

parseFilterOps :: ActionT AppM FilterOps
parseFilterOps = do
  params <- captureParams <&> map (bimapBoth toStrict)

  pure $ FilterOps
    { filterTag    =  lookup "tag"       params
    , filterAuthor =  lookup "author"    params
    , filterFavBy  =  lookup "favorited" params
    , filterLimit  = (lookup "limit"     params >>= toString -. readMaybe) ?: 20
    , filterOffset = (lookup "offset"    params >>= toString -. readMaybe) ?: 0
    }

class (Monad m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireArticles m where
  findArticles :: Maybe UserID -> FilterOps -> m (Either ArticleError ManyArticles)
  findArticles userID FilterOps {..} = mapDBResult toManyArticles <$> runDB do
    select $ do
      (a :& u) <- from $
        table @Article
          `leftJoin`
        table @User
          `on` \(a :& u) ->
            just a.author ==. u.id

      groupBy (u.id, a.id)

      whenJust filterAuthor $ \name -> where_ $ u.username ==. just (val name)

      whenJust filterFavBy $ \name -> where_ $
        exists $ do
          (_ :& u') <- from $
            table @Favorite
              `leftJoin`
            table @User
              `on` \(f :& u') ->
                just f.user ==. u' ?. #id

          where_ $ u'.username ==. just (val name)

      -- temp until I figure out how to get persistent to work with goddamn arrays
      -- I know this is a vulnerability, but I'll deal with it later.
      whenJust filterTag $ \tag' -> where_ $ unsafeSqlValue ("'s" <> TB.fromText tag' <> "' = ANY(replace(replace(tags, '[', '{'), ']', '}')::text[])")

      limit  filterLimit
      offset filterOffset

      let favorited = exists $ void $ from (table @Favorite)
            `suchThat` \f' ->
              (a.id ==. f'.article) &&. (just f'.user ==. val (userID <&> id2sqlKey))

      let numFavs = subSelectCount @Int $ from (table @Favorite)
            `suchThat` \f' ->
              a.id ==. f'.article

      let follows = exists $ void $ from (table @Follow)
            `suchThat` \f' ->
              (just f'.followerID ==. val (userID <&> id2sqlKey)) &&. (just f'.followeeID ==. u.id)

      orderBy [E.desc a.created]

      pure (a, u, follows, favorited, numFavs)

toManyArticles :: [(Entity Article, Maybe (Entity User), Value Bool, Value Bool, Value Int)] -> ManyArticles
toManyArticles = ManyArticles . map toOneArticle

toOneArticle :: (Entity Article, Maybe (Entity User), Value Bool, Value Bool, Value Int) -> OneArticle
toOneArticle (_, Nothing, _, _, _) = error "I'll deal with this later..."
toOneArticle (Entity _ Article {..}, Just (Entity _ User {..}), Value follows, Value faved, Value numFavs) = OneArticle
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
    , userFollowed = follows
    }
  }
