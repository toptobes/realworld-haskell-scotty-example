{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances #-}

module Conduit.Features.Articles.DB where

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Time (UTCTime)
import Conduit.Features.Account.DB (UserId)
import Conduit.DB (zeroTime)

share [mkPersist sqlSettings, mkMigrate "migrateArticleTables"] [persistLowerCase|
  Article
    author  UserId
    slug     Text
    title    Text
    desc     Text
    body     Text
    tags    [Text]
    created UTCTime default=now()
    updated UTCTime default=now()
    UniqueSlug slug

  Favorite
    user UserId
    article ArticleId
    Primary article user

  Comment
    author  UserId
    body    Text
    created UTCTime default=now()
    updated UTCTime default=now()
|]

mkArticle :: UserId -> Text -> Text -> Text -> Text -> [Text] -> Article
mkArticle author slug title desc body tags = Article author slug title desc body tags zeroTime zeroTime
