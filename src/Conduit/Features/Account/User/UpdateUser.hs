{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.UpdateUser where

import Prelude hiding (put)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB(..))
import Conduit.Errors (FeatureErrorHandler(..), mapDBError)
import Conduit.Features.Account.User.GetUser (AcquireUser, tryGetUser)
import Conduit.Features.Account.DB (EntityField(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID(..), inUserObj)
import Conduit.Identity.Auth (AuthTokenGen(..), AuthedUser (..), withAuth)
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (set, update, val, valkey, where_, (=.), (==.), (^.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, put)
import Conduit.Utils (InObj(InObj))

data UpdateUserAction = UpdateUserAction
  { username :: Maybe Text
  , password :: Maybe UnsafePassword
  , email    :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, FromJSON)

handleUpdateUser :: ScottyT AppM ()
handleUpdateUser = put "/api/user" $ withAuth \user -> do
  (InObj _ action) <- jsonData
  userAuth <- liftApp (tryUpdateUser user action)
  withFeatureErrorsHandled userAuth $
    json . inUserObj

tryUpdateUser :: (PasswordGen m, AuthTokenGen m, AcquireUser m, UpdateUser m) => AuthedUser -> UpdateUserAction -> m (Either AccountError UserAuth)
tryUpdateUser user@AuthedUser {..} action = runExceptT do
  maybeNewPW <- mapM (lift . hashPassword) action.password
  ExceptT $ updateUser authedUserID $ mkToUpdate action maybeNewPW
  ExceptT $ tryGetUser user

mkToUpdate :: UpdateUserAction -> Maybe HashedPassword -> ToUpdate
mkToUpdate UpdateUserAction {..} hashed = ToUpdate username hashed email bio image

class (Monad m) => UpdateUser m where
  updateUser :: UserID -> ToUpdate -> m (Either AccountError ())

data ToUpdate = ToUpdate
  { userName  :: Maybe Text
  , userPass  :: Maybe HashedPassword
  , userEmail :: Maybe Text
  , userBio   :: Maybe Text
  , userImage :: Maybe Text
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => UpdateUser m where
  updateUser :: UserID -> ToUpdate -> m (Either AccountError ())
  updateUser userID ToUpdate {..} = mapDBError <$> runDB do
    update $ \u -> do
      whenJust userName  \new -> set u [ UserUsername =. val new           ]
      whenJust userPass  \new -> set u [ UserPassword =. val new.getHashed ]
      whenJust userEmail \new -> set u [ UserEmail    =. val new           ]
      whenJust userBio   \new -> set u [ UserBio      =. val (Just new) ]
      whenJust userImage \new -> set u [ UserImage    =. val (Just new) ]
      where_ (u ^. #id ==. valkey userID.unID)
