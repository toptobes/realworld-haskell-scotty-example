module Conduit.Features.Articles.Handlers where

import Conduit.App.Monad (AppM)
import Web.Scotty.Internal.Types (ScottyT)

import Conduit.Features.Articles.Articles.CreateArticle      (handleArticleCreation)
import Conduit.Features.Articles.Articles.GetArticle         (handleGetArticle)
import Conduit.Features.Articles.Favorites.FavoriteArticle   (handleArticleFavorite)
import Conduit.Features.Articles.Favorites.UnfavoriteArticle (handleArticleUnfavorite)
import Conduit.Features.Articles.Articles.UpdateArticle      (handleArticleUpdate)
import Conduit.Features.Articles.Articles.DeleteArticle      (handleArticleDelete)
import Conduit.Features.Articles.Articles.ListArticles       (handleListArticles)
import Conduit.Features.Articles.Articles.FeedArticles       (handleFeedArticles)
import Conduit.Features.Articles.Tags.GetTags                (handleGetTags)
import Conduit.Features.Articles.Comments.AddComment         (handleCommentCreation)
import Conduit.Features.Articles.Comments.GetComments        (handleGetComments)
import Conduit.Features.Articles.Comments.DeleteComment      (handleCommentDeletion)

handlers :: ScottyT AppM ()
handlers = fold
  [ handleArticleCreation
  , handleArticleFavorite
  , handleArticleUnfavorite
  , handleArticleUpdate
  , handleArticleDelete
  , handleFeedArticles
  , handleListArticles
  , handleGetArticle
  , handleGetTags
  , handleCommentCreation
  , handleGetComments
  , handleCommentDeletion
  ]
