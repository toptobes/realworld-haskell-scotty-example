{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.RegisterUser where

import Prelude hiding (pass)
import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), mapDBResult, sqlKey2ID)
import Conduit.Features.Account.Common.EnsureUserCredsUnique (ReadUsers, ensureUserCredsUnique)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID(..), inUserObj)
import Conduit.Identity.Auth (AuthTokenGen(..))
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword(..))
import Conduit.Validation (NotBlank(..), parseJsonBody, (<!<))
import Data.Aeson (FromJSON(..), withObject, (.:))
import Database.Esqueleto.Experimental (insert)
import Network.HTTP.Types (status201)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post, status)

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  }

instance FromJSON RegisterUserAction where
  parseJSON = withObject "RegisterUserAction" $ \v -> RegisterUserAction
    <$> v .: "username"  <!< NotBlank
    <*> v .: "password"  <!< NotBlank
    <*> v .: "email"     <!< NotBlank

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  action <- parseJsonBody
  user <- runService $ registerUser action
  status status201
  json $ inUserObj user

defaultImage :: Text
defaultImage = "https://api.realworld.io/images/smiley-cyrus.jpeg"

registerUser :: (PasswordGen m, AuthTokenGen m, CreateUser m, ReadUsers m) => RegisterUserAction -> m (Either AccountError UserAuth)
registerUser RegisterUserAction {..} = runExceptT do
  ExceptT $ ensureUserCredsUnique (Just username) (Just email)

  hashedPass <- lift $ hashPassword password

  userID <- ExceptT $ insertUser UserInfo
    { name  = username
    , pass  = hashedPass
    , email = email
    }

  token <- lift $ mkAuthToken userID

  pure UserAuth
    { token = token
    , name  = username
    , email = email
    , bio   = Nothing
    , image = defaultImage
    }

class (Monad m) => CreateUser m where
  insertUser :: UserInfo -> m (Either AccountError UserID)

data UserInfo = UserInfo
  { name  :: !Text
  , pass  :: !HashedPassword
  , email :: !Text
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateUser m where
  insertUser :: UserInfo -> m (Either AccountError UserID)
  insertUser UserInfo {..} = mapDBResult sqlKey2ID <$> runDB do
    insert (User name pass.getHashed email mempty defaultImage)
