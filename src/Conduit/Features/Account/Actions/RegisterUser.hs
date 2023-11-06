{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.DB (UserTable (..), usersTable)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth (..), UserID (..))
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword (..), PasswordGen (..), UnsafePassword)
import Conduit.Utils ((-.))
import Data.Aeson (FromJSON)
import Database.Selda (def, fromId, insertWithPK)
import Database.Selda.Backend (MonadSelda)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  (InUserObj action) <- jsonData
  json =<< liftApp (registerUser action)

registerUser :: (PasswordGen m, CreateUser m, AuthTokenGen m) => RegisterUserAction -> m (InUserObj UserAuth)
registerUser RegisterUserAction {..} = do
  hashedPass <- hashPassword password

  userID <- createUser UserInfo
    { userName  = username
    , userPass  = hashedPass
    , userEmail = email
    }

  token <- mkAuthToken userID

  pure $ InUserObj UserAuth
    { userToken = token
    , userName  = username
    , userEmail = email
    , userBio   = Nothing
    , userImage = Nothing
    }

class (Monad m) => CreateUser m where
  createUser :: UserInfo -> m UserID

data UserInfo = UserInfo
  { userName  :: !Text
  , userPass  :: !HashedPassword
  , userEmail :: !Text
  }

instance (Monad m, MonadSelda m) => CreateUser m where
  createUser :: UserInfo -> m UserID
  createUser UserInfo {..} = insertWithPK usersTable [user] <&> fromId -. fromIntegral -. UserID 
    where 
      user = UserTable 
        { user_id = def, email = userEmail,   image = Nothing 
        , bio = Nothing, username = userName, password = userPass.getHashedPassword
        }
