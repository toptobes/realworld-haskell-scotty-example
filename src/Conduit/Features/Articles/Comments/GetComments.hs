{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Comments.GetComments where

import Prelude hiding (get)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB, sqlKey2ID, runDB)
import Conduit.Features.Account.DB (User, mkProfile)
import Conduit.Features.Account.Common.QueryAssociatedUser (queryAssociatedUser)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.DB (Comment(..))
import Conduit.Features.Articles.Errors (ArticleError)
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID(..), ManyComments(..), OneComment(..), Slug(..))
import Conduit.Identity.Auth (authedUserID, maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), Value, select, valkey, where_, (:&)(..), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)

handleGetComments :: ScottyT AppM ()
handleGetComments = get "/api/articles/:slug/comments" $ maybeWithAuth \user -> do
  slug <- captureParam "slug" <&> Slug
  comment <- liftApp (getComments slug (user <&> authedUserID))
  withFeatureErrorsHandled comment json

getComments :: (AquireComment m) => Slug -> Maybe UserID -> m (Either ArticleError ManyComments)
getComments slug userID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  ExceptT $ findCommentsForArticle articleID userID

class (Monad m) => AquireComment m where
  findCommentsForArticle :: ArticleID -> Maybe UserID -> m (Either ArticleError ManyComments)

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireComment m where
  findCommentsForArticle :: ArticleID -> Maybe UserID -> m (Either ArticleError ManyComments)
  findCommentsForArticle articleID userID = mapDBResult toManyComments <$> runDB do
    select $ do
      (c :& u, follows) <- queryAssociatedUser userID \c u -> 
        c.author ==. u.id

      where_ $ c.article ==. valkey articleID.unID

      pure (c, u, follows)

toManyComments :: [(Entity Comment, Entity User, Value Bool)] -> ManyComments
toManyComments = ManyComments . map toOneComment

toOneComment :: (Entity Comment, Entity User, Value Bool) -> OneComment
toOneComment (Entity commentID comment, user, follows) = OneComment
  { commentID = commentID & sqlKey2ID
  , body = comment.commentBody
  , created = comment.commentCreated
  , updated = comment.commentUpdated
  , author = mkProfile user follows
  }
