{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.LoginUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.DB (UserTable, usersTable)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth (..), UserID)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Data.Aeson (FromJSON)
import Database.Selda (ID, MonadSelda, fromId, query, restrict, select, (!), (.==), (:*:) (..))
import Database.Selda qualified as S
import UnliftIO (stringException)
import UnliftIO.Exception (throwIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)

data LoginUserAction = LoginUserAction
  { password :: Text
  , email :: Text
  } deriving (Show, Generic, FromJSON)

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  (InUserObj action) <- jsonData
  json =<< liftApp (loginUser action)

loginUser :: (AcquireUser m, AuthTokenGen m) => LoginUserAction -> m (InUserObj UserAuth)
loginUser LoginUserAction {..} = do
  userInfo <- findUserByEmail email
  token <- mkAuthToken userInfo.userID

  pure $ InUserObj UserAuth
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

instance (Monad m, MonadSelda m) => AcquireUser m where
  findUserByEmail :: Text -> m UserInfo
  findUserByEmail email = do
    let emailCol = S.text email

    extractUserInfo =<< query do
      u <- select usersTable
      restrict (u ! #email .== emailCol)
      pure (u ! #user_id :*: u ! #username :*: u ! #email :*: u ! #bio :*: u ! #image)

extractUserInfo :: (MonadIO m) => [ID UserTable :*: Text :*: Text :*: Maybe Text :*: Maybe Text] -> m UserInfo
extractUserInfo [] = liftIO . throwIO $ stringException "go away"
extractUserInfo [userID :*: name :*: email :*: bio :*: image] = pure $ UserInfo (fromIntegral $ fromId userID) name email bio image
extractUserInfo _ = error "should never happen b/c of unique email restriction"
