{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.RegisterUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.Types (UserAuth(..), UserID(..))
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Data.Aeson (FromJSON)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)
import Database.Selda (insertWithPK, fromId, def)
import Conduit.Utils ((-.))
import Conduit.Features.Account.DB (usersTable, UserTable(..))
import Database.Selda.Backend (MonadSelda)
import Conduit.Identity.Password (HashedPassword(..), UnsafePassword, PasswordGen(..))

data RegisterUserAction = RegisterUserAction
  { username :: Text
  , password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserRegistration :: ScottyT AppM ()
handleUserRegistration = post "/api/users" do
  action <- jsonData @RegisterUserAction
  user <- liftApp $ registerUser action
  json user

registerUser :: (PasswordGen m, CreateUser m, AuthTokenGen m) => RegisterUserAction -> m UserAuth
registerUser RegisterUserAction {..} = do
  hashedPass <- hashPassword password

  userID <- createUser UserInfo
    { userName  = username
    , userPass  = hashedPass
    , userEmail = email
    }

  token <- mkAuthToken userID

  pure UserAuth
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
        { userID = def,      userName = userName,   userPass = userPass.getHashedPassword
        , userBio = Nothing, userEmail = userEmail, userImage = Nothing 
        }
