module Conduit.Features.Articles.Handlers where

import Conduit.App.Monad (AppM)
import Web.Scotty.Internal.Types (ScottyT)

import Conduit.Features.Articles.Articles.CreateArticle      (handleArticleCreation)
import Conduit.Features.Articles.Articles.GetArticle         (handleGetArticle)
import Conduit.Features.Articles.Favorites.FavoriteArticle   (handleArticleFavorite)
import Conduit.Features.Articles.Favorites.UnfavoriteArticle (handleArticleUnfavorite)
import Conduit.Features.Articles.Articles.UpdateArticle      (handleArticleUpdate)
import Conduit.Features.Articles.Articles.DeleteArticle      (handleArticleDelete)
import Conduit.Features.Articles.Articles.ListArticles       (handleGetArticles)

handlers :: ScottyT AppM ()
handlers = fold
  [ handleArticleCreation
  , handleGetArticle
  , handleArticleFavorite
  , handleArticleUnfavorite
  , handleArticleUpdate
  , handleArticleDelete
  , handleGetArticles
  ]
