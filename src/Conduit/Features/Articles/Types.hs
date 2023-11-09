module Conduit.Features.Articles.Types where

import Data.Aeson (ToJSON(toJSON), object, (.=))
import Data.Aeson.Types (Value)
import Data.Time (UTCTime)
import Conduit.Features.Account.Types (UserProfile)
import Conduit.Utils (InObj(InObj))

newtype ArticleID = ArticleID { unID :: Int64 } 
  deriving newtype (Show, Read, Eq, ToJSON)

inAuthorObj :: obj -> InObj obj
inAuthorObj = InObj "author"

data OneArticle = OneArticle
  { author    :: InObj UserProfile
  , slug      :: Text
  , title     :: Text
  , desc      :: Text
  , body      :: Text
  , tags      :: [Text]
  , favorited :: Bool
  , numFavs   :: Int
  , created   :: UTCTime
  , updated   :: UTCTime
  }

instance ToJSON OneArticle where
  toJSON :: OneArticle -> Value
  toJSON OneArticle {..} = object
    [ "slug"           .= slug
    , "title"          .= title
    , "description"    .= desc
    , "body"           .= body
    , "tagList"        .= tags
    , "createdAt"      .= created
    , "updatedAt"      .= updated
    , "favorited"      .= favorited
    , "favoritedCount" .= numFavs
    , "author"         .= author
    ]

inArticleObj :: obj -> InObj obj
inArticleObj = InObj "article"

newtype ManyArticles = MultipleArticles
  { articles :: [OneArticle]
  }

instance ToJSON ManyArticles where
  toJSON :: ManyArticles -> Value
  toJSON MultipleArticles {..} = object
    [ "articles" .= articles
    , "articlesCount" .= length articles
    ]
