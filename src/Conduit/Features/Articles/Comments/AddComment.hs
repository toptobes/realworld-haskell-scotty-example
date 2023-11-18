{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Comments.AddComment where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (FeatureErrorHandler(..), mapDBResult)
import Conduit.DB.Types (MonadDB, SqlKey (id2sqlKey, sqlKey2ID), runDB)
import Conduit.DB.Utils (zeroTime)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.DB (Comment(..))
import Conduit.Features.Articles.Errors (ArticleError (CommentNotFoundEx))
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID, CommentID, OneComment(..), Slug(..), inCommentObj, ManyComments (..))
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Utils (InObj(..))
import Database.Esqueleto.Experimental (insert)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, jsonData, post)
import Conduit.Features.Articles.Comments.GetComments (AquireComment, getComments)
import Data.Aeson (FromJSON)

newtype CreateCommentAction = CreateCommentAction
  { body :: Text
  } deriving (Generic)
    deriving anyclass (FromJSON)

handleCommentCreation :: ScottyT AppM ()
handleCommentCreation = post "/api/articles/:slug/comments" $ withAuth \user -> do
  (InObj _ body) <- jsonData
  slug <- captureParam "slug" <&> Slug
  comment <- liftApp (createComment body slug user.authedUserID)
  withFeatureErrorsHandled comment $
    json . inCommentObj

createComment :: (AquireComment m, CreateComment m) => CreateCommentAction -> Slug -> UserID -> m (Either ArticleError OneComment)
createComment CreateCommentAction {..} slug authorID = runExceptT do
  articleID <- ExceptT . pure $ extractIDFromSlug slug
  commentID <- ExceptT $ insertComment CommentInfo 
    { authorID  = authorID
    , articleID = articleID
    , body      = body
    }
  ManyComments comments <- ExceptT $ getComments slug (Just authorID)
  ExceptT . pure . maybeToRight CommentNotFoundEx $ find (\c -> c.commentID == commentID) comments -- def don't do this in actual code, I'm just lazy :)

class (Monad m) => CreateComment m where
  insertComment :: CommentInfo -> m (Either ArticleError CommentID)

data CommentInfo = CommentInfo
  { authorID  :: UserID
  , articleID :: ArticleID
  , body      :: Text
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateComment m where
  insertComment :: CommentInfo -> m (Either ArticleError CommentID)
  insertComment CommentInfo {..} = mapDBResult sqlKey2ID <$> runDB do
    insert $ mkComment authorID articleID body

mkComment :: UserID -> ArticleID -> Text -> Comment
mkComment (id2sqlKey -> authorID) (id2sqlKey -> articleID) body = Comment authorID articleID body zeroTime zeroTime
