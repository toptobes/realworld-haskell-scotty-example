{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..), sqlKey2ID)
import Conduit.Features.Account.Common.EnsureUserCredsUnique (ReadUsers, ensureUserCredsUnique)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID(..), inUserObj)
import Conduit.Identity.Auth (AuthTokenGen(..))
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword(..))
import Conduit.Utils ((>->))
import Conduit.Validation (Validations, are, fromJsonObj, notBlank)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (insert)
import Network.HTTP.Types (status201)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post, status)

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

validations :: Validations RegisterUserAction
validations RegisterUserAction {..} =
  [ (username,           "username")
  , (password.getUnsafe, "password")
  , (email,              "email")
  ] `are` notBlank

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  action <- fromJsonObj validations
  user' <- liftApp (registerUser action)
  withFeatureErrorsHandled user' $
    json . inUserObj >->
    status status201

defaultImage :: Text
defaultImage = "https://api.realworld.io/images/smiley-cyrus.jpeg"

registerUser :: (PasswordGen m, CreateUser m, ReadUsers m, AuthTokenGen m) => RegisterUserAction -> m (Either AccountError UserAuth)
registerUser RegisterUserAction {..} = runExceptT do
  ExceptT $ ensureUserCredsUnique (Just username) (Just email)

  hashedPass <- lift $ hashPassword password

  userID <- ExceptT $ insertUser UserInfo
    { userName  = username
    , userPass  = hashedPass
    , userEmail = email
    }

  token <- lift $ mkAuthToken userID

  pure UserAuth
    { userToken = token
    , userName  = username
    , userEmail = email
    , userBio   = Nothing
    , userImage = defaultImage
    }

class (Monad m) => CreateUser m where
  insertUser :: UserInfo -> m (Either AccountError UserID)

data UserInfo = UserInfo
  { userName  :: !Text
  , userPass  :: !HashedPassword
  , userEmail :: !Text
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateUser m where
  insertUser :: UserInfo -> m (Either AccountError UserID)
  insertUser UserInfo {..} = mapDBResult sqlKey2ID <$> runDB do
    insert (User userName userPass.getHashed userEmail mempty defaultImage)
