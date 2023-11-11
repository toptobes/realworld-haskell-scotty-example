{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), sqlKey2ID)
import Conduit.DB.Errors (mapDBResult, withFeatureErrorsHandled)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID(..), inUserObj)
import Conduit.Identity.Auth (AuthTokenGen(..))
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (PersistStoreWrite (insert))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)
import Conduit.Utils (InObj(InObj))

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  (InObj _ action) <- jsonData
  user' <- liftApp (registerUser action)
  withFeatureErrorsHandled user' $
    json . inUserObj

registerUser :: (PasswordGen m, CreateUser m, AuthTokenGen m) => RegisterUserAction -> m (Either AccountError UserAuth)
registerUser RegisterUserAction {..} = runExceptT do
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
    , userImage = Nothing
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
    insert (User userName userPass.getHashed userEmail mempty mempty)
