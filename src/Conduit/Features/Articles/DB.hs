{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances #-}

module Conduit.Features.Articles.DB where

import Conduit.DB.Types (SqlKey(..))
import Conduit.DB.Utils (zeroTime, suchThat)
import Conduit.Features.Account.DB (UserId)
import Conduit.Features.Articles.Types (ArticleID(..))
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Key, PersistEntity(..), SqlPersistT, rawExecute, from, table, (==.), valkey, (&&.), selectOne,)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.FileEmbed (embedFile)
import Conduit.Features.Account.Types (UserID (UserID))
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
    author  UserId OnDeleteCascade
    body    Text
    created UTCTime default=now()
    updated UTCTime default=now()
|]

createArticleFunctions :: (MonadIO m) => SqlPersistT m ()
createArticleFunctions = rawExecute fns [] where
  fns = decodeUtf8 $ fold 
    [ $(embedFile "sql/articles/slug_id_prepender.sql")
    , $(embedFile "sql/articles/set_timestamps.sql")
    ]

mkArticle :: UserId -> Text -> Text -> Text -> Text -> [Text] -> Article
mkArticle author slug title desc body tags = Article author slug title desc body tags zeroTime zeroTime

instance SqlKey Article ArticleID where
  sqlKey2ID :: Key Article -> ArticleID
  sqlKey2ID = ArticleID . E.fromSqlKey

  id2sqlKey :: ArticleID -> Key Article
  id2sqlKey = E.toSqlKey . unID

userOwnsArticle :: (MonadIO m) => UserID -> ArticleID -> SqlPersistT m Bool
userOwnsArticle (UserID userID) (ArticleID articleID) = isJust <$> result where
  result = selectOne $ void $ from (table @Article)
    `suchThat` \a ->
      (a.author ==. valkey userID) &&. (a.id ==. valkey articleID)

assumingUserOwnsArticle :: (MonadIO m) => UserID -> ArticleID -> SqlPersistT m b -> SqlPersistT m b
assumingUserOwnsArticle u a action = do
  authorized <- userOwnsArticle u a

  if authorized
    then action
    else throwIO $ authorizationSqlError "user attempted to manipulate unowned article"
