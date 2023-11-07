{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.LoginUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (catchErrorInto)
import Conduit.Features.Account.DB (UserTable, usersTable)
import Conduit.Features.Account.Errors (AccountError(..), withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth(..), UserID)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), UnsafePassword, testPassword)
import Data.Aeson (FromJSON)
import Database.Selda (ID, MonadSelda, fromId, query, restrict, select, (!), (.==), (:*:)(..))
import Database.Selda qualified as S
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)

data LoginUserAction = LoginUserAction
  { password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  (InUserObj action) <- jsonData
  userAuth <- liftApp (tryLoginUser action)
  withAccountErrorsHandled userAuth $
    json . InUserObj

tryLoginUser :: (MonadIO m, AcquireUser m, AuthTokenGen m) => LoginUserAction -> m (Either AccountError UserAuth)
tryLoginUser LoginUserAction {..} = runExceptT do
  user <- ExceptT $ findUserByEmail email

  let isValidPassword = testPassword password user.userPass

  either' <- lift $ if isValidPassword
    then Right <$> createUserAuth user
    else pure . Left $ UserUnauthorizedEx

  ExceptT $ pure either'

createUserAuth :: (AuthTokenGen m) => UserInfo -> m UserAuth
createUserAuth userInfo = do
  token <- mkAuthToken userInfo.userID

  pure UserAuth
    { userToken = token
    , userEmail = userInfo.userEmail
    , userName  = userInfo.userName
    , userBio   = userInfo.userBio
    , userImage = userInfo.userImage
    }

class (Monad m) => AcquireUser m where
  findUserByEmail :: Text -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userID    :: !UserID
  , userName  :: !Text
  , userEmail :: !Text
  , userPass  :: HashedPassword
  , userBio   :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }

instance (Monad m, MonadUnliftIO m, MonadSelda m) => AcquireUser m where
  findUserByEmail :: Text -> m (Either AccountError UserInfo)
  findUserByEmail email = runExceptT do
    let emailCol = S.text email

    result <- ExceptT $ catchErrorInto SomeSQLError $ query do
      u <- select usersTable
      restrict (u ! #email .== emailCol)
      pure (u ! #user_id :*: u ! #username :*: u ! #email :*: u ! #password :*: u ! #bio :*: u ! #image)

    ExceptT . pure $ extractUserInfo result

extractUserInfo :: [ID UserTable :*: Text :*: Text :*: Text :*: Maybe Text :*: Maybe Text] -> Either AccountError UserInfo
extractUserInfo [] = Left UserNotFoundEx
extractUserInfo [userID :*: name :*: email :*: password :*: bio :*: image] = 
  pure $ UserInfo (fromIntegral $ fromId userID) name email (HashedPassword password) bio image
extractUserInfo _ = error "should never happen b/c of unique email restriction"
