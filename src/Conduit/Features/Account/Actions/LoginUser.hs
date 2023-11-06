{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.LoginUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.Types (UserAuth(..), UserID)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Data.Aeson (FromJSON)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)
import Conduit.App.Has (Has)
import Conduit.DB (DBPool)

data LoginUserAction = LoginUserAction
  { password :: Text
  , email :: Text
  } deriving (Show, Generic, FromJSON)

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  action <- jsonData @LoginUserAction
  user <- liftApp $ loginUser action
  json user

loginUser :: (AcquireUser m, AuthTokenGen m) => LoginUserAction -> m UserAuth
loginUser LoginUserAction {..} = do
  userInfo <- findUserByEmail email
  token <- mkAuthToken userInfo.userID

  pure UserAuth
    { userToken = token
    , userEmail = userInfo.userEmail
    , userName  = userInfo.userName
    , userBio   = userInfo.userBio
    , userImage = userInfo.userImage
    }

class (Monad m) => AcquireUser m where
  findUserByEmail :: Text -> m UserInfo

data UserInfo = UserInfo
  { userID    :: !UserID
  , userName  :: !Text
  , userEmail :: !Text
  , userBio   :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }

instance (Monad m, MonadReader c m, Has DBPool c) => AcquireUser m where
  findUserByEmail :: Text -> m UserInfo
  findUserByEmail = undefined
