{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (catchErrorInto)
import Conduit.Features.Account.DB (UserTable(..), usersTable)
import Conduit.Features.Account.Errors (AccountError(..), withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth(..), UserID(..))
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), PasswordGen(..), UnsafePassword)
import Conduit.Utils ((-.))
import Data.Aeson (FromJSON)
import Database.Selda (def, fromId, insertWithPK)
import Database.Selda.Backend (MonadSelda)
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
registerUser RegisterUserAction {..} = runExceptT $ do
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

instance (Monad m, MonadSelda m, MonadUnliftIO m) => CreateUser m where
  createUser :: UserInfo -> m (Either AccountError UserID)
  createUser UserInfo {..} = catchErrorInto DetailTakenEx $ insertWithPK usersTable [user] <&> fromId -. fromIntegral -. UserID
    where
      user = UserTable
        { user_id = def, email = userEmail,   image = Nothing
        , bio = Nothing, username = userName, password = userPass.getHashedPassword
        }
