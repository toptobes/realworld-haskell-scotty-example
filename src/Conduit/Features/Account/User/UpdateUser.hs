{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.UpdateUser where

import Prelude hiding (put, pass)
import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB (..), mapDBError)
import Conduit.Features.Account.Common.EnsureUserCredsUnique (ReadUsers, ensureUserCredsUnique)
import Conduit.Features.Account.DB (User)
import Conduit.Features.Account.Errors (AccountError (..))
import Conduit.Features.Account.Types (UserAuth (..), UserID (..), inUserObj)
import Conduit.Features.Account.User.GetUser (AcquireUser, tryGetUser)
import Conduit.Identity.Auth (AuthTokenGen (..), AuthedUser (..), withAuth)
import Conduit.Identity.Password (HashedPassword (..), PasswordGen (..), UnsafePassword (..))
import Conduit.Val (NotBlank (..), fromJsonObj, (<?!<))
import Data.Aeson (FromJSON (..), withObject, (.:?))
import Database.Esqueleto.Experimental (set, update, val, valkey, where_, (=.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, put)

data UpdateUserAction = UpdateUserAction
  { username :: Maybe Text
  , password :: Maybe UnsafePassword
  , email    :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  }

instance FromJSON UpdateUserAction where
  parseJSON = withObject "UpdateUserAction" $ \v -> UpdateUserAction
    <$> v .:? "username" <?!< NotBlank
    <*> v .:? "password" <?!< NotBlank
    <*> v .:? "email"    <?!< NotBlank
    <*> v .:? "bio"      <?!< NotBlank
    <*> v .:? "image"    <?!< NotBlank

handleUpdateUser :: ScottyT AppM ()
handleUpdateUser = put "/api/user" $ withAuth \user -> do
  action <- fromJsonObj
  userAuth <- runService $ updateUser user action
  json $ inUserObj userAuth

updateUser :: (PasswordGen m, AuthTokenGen m, AcquireUser m, ReadUsers m, UpdateUser m) => AuthedUser -> UpdateUserAction -> m (Either AccountError UserAuth)
updateUser user@AuthedUser {..} action@UpdateUserAction {..} = runExceptT do
  ExceptT $ ensureUserCredsUnique username email
  maybeNewPW <- mapM (lift . hashPassword) password
  ExceptT $ updateUserByID authedUserID $ mkToUpdate action maybeNewPW
  ExceptT $ tryGetUser user

mkToUpdate :: UpdateUserAction -> Maybe HashedPassword -> ToUpdate
mkToUpdate UpdateUserAction {..} hashed = ToUpdate username hashed email bio image

class (Monad m) => UpdateUser m where
  updateUserByID :: UserID -> ToUpdate -> m (Either AccountError ())

data ToUpdate = ToUpdate
  { name  :: Maybe Text
  , pass  :: Maybe HashedPassword
  , email :: Maybe Text
  , bio   :: Maybe Text
  , image :: Maybe Text
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => UpdateUser m where
  updateUserByID :: UserID -> ToUpdate -> m (Either AccountError ())
  updateUserByID userID ToUpdate {..} = mapDBError <$> runDB do
    update @_ @User $ \u -> do
      whenJust name  \new -> set u [ #username =. val new           ]
      whenJust pass  \new -> set u [ #password =. val new.getHashed ]
      whenJust email \new -> set u [ #email    =. val new           ]
      whenJust bio   \new -> set u [ #bio      =. val (Just new)    ]
      whenJust image \new -> set u [ #image    =. val new           ]
      where_ (u.id ==. valkey userID.unID)
