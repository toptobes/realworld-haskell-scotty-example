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

import Conduit.DB.Core (authorizationSqlError, resourceNotFoundSqlError)
import Conduit.DB.Core (SqlKey(..), deriveSqlKey)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (User, UserId, mkProfile)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Types (ArticleID(..), CommentID(..), ManyArticles(..), OneArticle(..), Slug(..))
import Data.FileEmbed (embedFile)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity(..), PersistEntity(..), SqlExpr, SqlPersistT, Value(..), from, rawExecute, selectOne, table, val, (==.))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import UnliftIO.Exception (throwIO)

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
    UniqueSlug   slug
    UniqueTitle title

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

assumingUserIsOwner :: ∀ table id m a e. (OwnableEntity table, SqlKey table id, MonadIO m, Show e) => e -> UserID -> id -> SqlPersistT m a -> SqlPersistT m a
assumingUserIsOwner err userID tableID action = do
  result <- selectOne $ from (table @table)
    `suchThat` \a ->
      getID a ==. val (id2sqlKey tableID)

  case result of
    Nothing -> throwIO resourceNotFoundSqlError
    Just resource -> if getOwner resource == userID
      then action
      else throwIO $ authorizationSqlError err

class (PersistEntity table) => OwnableEntity table where
  getID :: SqlExpr (Entity table) -> SqlExpr (Value (Key table))
  getOwner :: Entity table -> UserID

instance OwnableEntity Article where
  getOwner (Entity _ a) = sqlKey2ID a.articleAuthor
  getID a = a.id

instance OwnableEntity Comment where
  getOwner (Entity _ c) = sqlKey2ID c.commentAuthor
  getID a = a.id

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
