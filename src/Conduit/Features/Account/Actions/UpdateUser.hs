{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.UpdateUser where

import Prelude hiding (put)
import Conduit.App.Monad (AppM, MonadApp (liftApp))
import Conduit.DB (catchErrorInto)
import Conduit.Features.Account.Actions.GetUser (AcquireUser, tryGetUser)
import Conduit.Features.Account.DB (usersTable)
import Conduit.Features.Account.Errors (AccountError(..), withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj(InUserObj), UserAuth(..), UserID(..))
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken), AuthedUser(..), withAuth)
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword)
import Data.Aeson (FromJSON)
import Database.Selda (Assignment((:=)), MonadSelda, update_, with, (!), (.==))
import Database.Selda qualified as S
import Relude.Unsafe as Unsafe
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, put)

data UpdateUserAction = UpdateUserAction
  { username :: Maybe Text
  , password :: Maybe UnsafePassword
  , email    :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, FromJSON)

handleUpdateUser :: ScottyT AppM ()
handleUpdateUser = put "/api/user" $ withAuth \user -> do
  (InUserObj action) <- jsonData
  userAuth <- liftApp (tryUpdateUser user action)
  withAccountErrorsHandled userAuth $
    json . InUserObj

tryUpdateUser :: (PasswordGen m, AuthTokenGen m, AcquireUser m, UpdateUser m) => AuthedUser -> UpdateUserAction -> m (Either AccountError UserAuth)
tryUpdateUser user@AuthedUser {..} action = runExceptT do
  (authToken, maybeNewPW) <- lift $ mkNewSecurityDetails user action.password

  ExceptT $ updateUser authedUserID $ mkToUpdate action maybeNewPW
  ExceptT $ tryGetUser user { authedToken = authToken }

mkNewSecurityDetails :: (AuthTokenGen m, PasswordGen m) => AuthedUser -> Maybe UnsafePassword -> m (Text, Maybe HashedPassword)
mkNewSecurityDetails user Nothing = pure (user.authedToken, Nothing)
mkNewSecurityDetails user (Just unsafePass) = do
  hashedPass <- hashPassword unsafePass
  token <- mkAuthToken user.authedUserID
  pure (token, Just hashedPass)

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

instance (Monad m, MonadUnliftIO m, MonadSelda m) => UpdateUser m where
  updateUser :: UserID -> ToUpdate -> m (Either AccountError ())
  updateUser userID ToUpdate {..} = runExceptT do
    let userIDCol = S.literal $ S.toId userID.unUserID
        unsafePass = getHashedPassword <$> userPass

 -- relies heavily on laziness but luckily I'm the laziest person I know
    let updates = filter (isNothing . fst)
          [ (userName,   #username := S.text (Unsafe.fromJust userName))
          , (unsafePass, #password := S.text (Unsafe.fromJust unsafePass))
          , (userEmail,  #email    := S.text (Unsafe.fromJust userEmail))
          , (userBio,    #bio      := S.just (S.text $ Unsafe.fromJust userBio))
          , (userImage,  #image    := S.just (S.text $ Unsafe.fromJust userImage))
          ]
    
    ExceptT $ catchErrorInto SomeSQLError $ update_ usersTable
      (\u -> u ! #user_id .== userIDCol)
      (\u -> u `with` (updates <&> snd))

filterCorresponding :: [Maybe a] -> [b] -> [b]
filterCorresponding maybes bs = [b | (Just _, b) <- zip maybes bs]
