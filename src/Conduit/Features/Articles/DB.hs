{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances, AllowAmbiguousTypes #-}

module Conduit.Features.Articles.DB
  ( migrateArticleTables
  , createArticleFunctions
  , assumingUserIsOwner
  , mkManyArticles
  , mkOneArticle
  , Article(..)
  , Favorite(..)
  , Comment(..)
  ) where

import Conduit.DB.Types (deriveSqlKey, SqlKey (id2sqlKey))
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (UserId, User, mkProfile)
import Conduit.Features.Articles.Types (ArticleID(..), CommentID(..), ManyArticles(..), OneArticle (..), Slug (..))
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlPersistT, rawExecute, from, table, (==.), (&&.), selectOne, PersistEntity (..), val, SqlExpr, Entity (..), Value (..))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.FileEmbed (embedFile)
import Conduit.Features.Account.Types (UserID)
import UnliftIO.Exception (throwIO)
import Conduit.DB.Errors (authorizationSqlError)

share [mkPersist sqlSettings, mkMigrate "migrateArticleTables"] [persistLowerCase|
  Article
    author  UserId  OnDeleteCascade
    slug     Text
    title    Text
    desc     Text
    body     Text
    tags    [Text]
    created UTCTime default=now()
    updated UTCTime default=now()
    UniqueSlug slug

  Favorite
    user    UserId    OnDeleteCascade
    article ArticleId OnDeleteCascade
    Primary article user

  Comment
    author  UserId    OnDeleteCascade
    article ArticleId OnDeleteCascade
    body    Text
    created UTCTime default=now()
    updated UTCTime default=now()
|]

createArticleFunctions :: (MonadIO m) => SqlPersistT m ()
createArticleFunctions = rawExecute fns [] where
  fns = decodeUtf8 $ fold 
    [ $(embedFile "sqlbits/articles/slug_id_prepender.sql")
    , $(embedFile "sqlbits/articles/set_timestamps.sql")
    ]

$(deriveSqlKey ''Article ''ArticleID)
$(deriveSqlKey ''Comment ''CommentID)

assumingUserIsOwner :: ∀ table id m a. (OwnableEntity table, SqlKey table id, MonadIO m) => Conduit.Features.Account.Types.UserID -> id -> SqlPersistT m a -> SqlPersistT m a
assumingUserIsOwner userID tableID action = do
  authorized <- selectOne $ void $ from (table @table)
    `suchThat` \a ->
      (getOwnerField a ==. val (id2sqlKey userID)) &&. (getIDField a ==. val (id2sqlKey tableID))

  if isJust authorized
    then action
    else throwIO authorizationSqlError

class (PersistEntity table) => OwnableEntity table where
  getOwnerField :: SqlExpr (Entity table) -> SqlExpr (Value (Key User))
  getIDField :: SqlExpr (Entity table) -> SqlExpr (Value (Key table))

instance OwnableEntity Article where
  getOwnerField a = a.author
  getIDField a = a.id

instance OwnableEntity Comment where
  getOwnerField a = a.author
  getIDField a = a.id

mkManyArticles :: [(Entity Article, Entity User, Value Bool, Value Bool, Value Int)] -> ManyArticles
mkManyArticles = ManyArticles . map mkOneArticle

mkOneArticle :: (Entity Article, Entity User, Value Bool, Value Bool, Value Int) -> OneArticle
mkOneArticle (Entity _ Article {..}, user, follows, Value faved, Value numFavs) = OneArticle
  { slug      = articleSlug & Slug
  , title     = articleTitle
  , tags      = articleTags & reverse
  , body      = articleBody
  , created   = articleCreated
  , updated   = articleUpdated
  , favorited = faved
  , numFavs   = numFavs
  , desc      = articleDesc
  , author    = mkProfile user follows
  }
