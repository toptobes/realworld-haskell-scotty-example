{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Comments.AddComment where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (withFeatureErrorsHandled, mapDBResult)
import Conduit.DB.Types (MonadDB, SqlKey (id2sqlKey, sqlKey2ID), runDB)
import Conduit.DB.Utils (dbTimeNow)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.Comments.GetComments (AquireComment, getComments)
import Conduit.Features.Articles.DB (Comment(..))
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Slugs (extractIDFromSlug)
import Conduit.Features.Articles.Types (ArticleID, CommentID, ManyComments(..), OneComment(..), Slug(..), inCommentObj)
import Conduit.Identity.Auth (authedUserID, withAuth)
import Conduit.Validation (Validations, fromJsonObj, is, notBlank)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (insert)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, post)

newtype CreateCommentAction = CreateCommentAction
  { body :: Text
  } deriving (Generic)
    deriving anyclass (FromJSON)

validations :: Validations CreateCommentAction
validations CreateCommentAction {..} =
  [ (body, "body") `is` notBlank
  ]

handleCommentCreation :: ScottyT AppM ()
handleCommentCreation = post "/api/articles/:slug/comments" $ withAuth \user -> do
  body <- fromJsonObj validations

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
  ExceptT . pure . maybeToRight ResourceNotFoundEx $ find (\c -> c.commentID == commentID) comments -- def don't do this in actual code, I'm just lazy :)

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
mkComment (id2sqlKey -> authorID) (id2sqlKey -> articleID) body = Comment authorID articleID body dbTimeNow dbTimeNow
