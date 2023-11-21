module Conduit.Features.Articles.Types where

import Conduit.Features.Account.Types (UserProfile)
import Conduit.Validation (InObj(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Types (Value)
import Data.Time (UTCTime)

newtype ArticleID = ArticleID { unID :: Int64 } 
  deriving newtype (Show, Read, Eq, ToJSON)

newtype Slug = Slug { unSlug :: Text }
  deriving (Show, Eq)
  deriving newtype (ToJSON)

newtype NoIDSlug = NoIDSlug { unSlug :: Text }
  deriving (Show, Eq)

inAuthorObj :: obj -> InObj obj
inAuthorObj = InObj "author"

data OneArticle = OneArticle
  { author    :: UserProfile
  , slug      :: Slug
  , title     :: Text
  , desc      :: Text
  , body      :: Text
  , tags      :: [Text]
  , favorited :: Bool
  , numFavs   :: Int
  , created   :: UTCTime
  , updated   :: UTCTime
  } deriving (Show)

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
    , "favoritesCount" .= numFavs
    , "author"         .= author
    ]

inArticleObj :: obj -> InObj obj
inArticleObj = InObj "article"

newtype ManyArticles = ManyArticles
  { articles :: [OneArticle]
  } deriving (Show)

instance ToJSON ManyArticles where
  toJSON :: ManyArticles -> Value
  toJSON ManyArticles {..} = object
    [ "articles" .= articles
    , "articlesCount" .= length articles
    ]

newtype CommentID = CommentID { unID :: Int64 } 
  deriving newtype (Show, Read, Eq, ToJSON)

data OneComment = OneComment
  { author    :: UserProfile
  , commentID :: CommentID
  , body      :: Text
  , created   :: UTCTime
  , updated   :: UTCTime
  } deriving (Show)

instance ToJSON OneComment where
  toJSON :: OneComment -> Value
  toJSON OneComment {..} = object
    [ "id"        .= commentID
    , "body"      .= body
    , "createdAt" .= created
    , "updatedAt" .= updated
    , "author"    .= author
    ]

inCommentObj :: obj -> InObj obj
inCommentObj = InObj "comment"

newtype ManyComments = ManyComments
  { comments :: [OneComment]
  } deriving (Show, Generic)
    deriving anyclass (ToJSON)

inTagsObj :: obj -> InObj obj
inTagsObj = InObj "tags"
