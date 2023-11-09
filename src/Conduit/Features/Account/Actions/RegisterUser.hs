{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB(..))
import Conduit.Features.Account.DB (User(..), sqlKey2userID)
import Conduit.Features.Account.Errors (AccountError(..), mapDBResult, withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth(..), UserID(..))
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (PersistStoreWrite (insert))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  (InUserObj action) <- jsonData
  user' <- liftApp (registerUser action)
  withAccountErrorsHandled user' $
    json . InUserObj

registerUser :: (PasswordGen m, CreateUser m, AuthTokenGen m) => RegisterUserAction -> m (Either AccountError UserAuth)
registerUser RegisterUserAction {..} = runExceptT do
  hashedPass <- lift $ hashPassword password

  userID <- ExceptT $ createUser UserInfo
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
    , userImage = Nothing
    }

class (Monad m) => CreateUser m where
  createUser :: UserInfo -> m (Either AccountError UserID)

data UserInfo = UserInfo
  { userName  :: !Text
  , userPass  :: !HashedPassword
  , userEmail :: !Text
  }

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateUser m where
  createUser :: UserInfo -> m (Either AccountError UserID)
  createUser UserInfo {..} = mapDBResult sqlKey2userID <$> runDB do
    insert (User userName userPass.getHashed userEmail mempty mempty)
