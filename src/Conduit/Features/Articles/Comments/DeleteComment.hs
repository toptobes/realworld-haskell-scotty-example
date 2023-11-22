{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Comments.DeleteComment where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), expectDBNonZero)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Features.Articles.DB (Comment, assumingUserIsOwner)
import Conduit.Features.Articles.Errors (ArticleError (..))
import Conduit.Features.Articles.Types (CommentID(..))
import Conduit.Identity.Auth (authedUserID, withAuth)
import Database.Esqueleto.Experimental (deleteCount, from, table, valkey, where_, (==.))
import Network.HTTP.Types (status200)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, status)
import Web.Scotty.Trans qualified as Scotty

handleCommentDeletion :: ScottyT AppM ()
handleCommentDeletion = Scotty.delete "/api/articles/:slug/comments/:id" $ withAuth \user -> do
  commentID <- captureParam "id" <&> CommentID
  runService $ deleteComment commentID user.authedUserID
  status status200

deleteComment :: (DeleteComment m) => CommentID -> UserID -> m (Either ArticleError ())
deleteComment commentID userID = runExceptT do
  ExceptT $ deleteCommentByID commentID userID

class (Monad m) => DeleteComment m where
  deleteCommentByID :: CommentID -> UserID -> m (Either ArticleError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => DeleteComment m where
  deleteCommentByID :: CommentID -> UserID -> m (Either ArticleError ())
  deleteCommentByID commentID userID = expectDBNonZero ResourceNotFoundEx <$> runDB do
    assumingUserIsOwner IllegalCommentDelEx userID commentID do
      deleteCount $ do
        a <- from (table @Comment)
        where_ (a.id ==. valkey commentID.unID)
