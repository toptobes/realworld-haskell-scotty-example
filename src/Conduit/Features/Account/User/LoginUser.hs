{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.LoginUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB(..))
import Conduit.Errors (FeatureErrorHandler(..), mapMaybeDBResult)
import Conduit.Features.Account.DB (EntityField (UserEmail), User(..), sqlKey2userID)
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID, inUserObj)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), UnsafePassword, testPassword)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (Entity(..), from, selectOne, val, where_, (==.), (^.))
import Database.Esqueleto.Experimental.From (table)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, jsonData, post)
import Conduit.Utils (InObj(InObj))

data LoginUserAction = LoginUserAction
  { password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  (InObj _ action) <- jsonData
  userAuth <- liftApp (tryLoginUser action)
  withFeatureErrorsHandled userAuth $
    json . inUserObj

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
  , userPass  :: !HashedPassword
  , userBio   :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => AcquireUser m where
  findUserByEmail :: Text -> m (Either AccountError UserInfo)
  findUserByEmail email = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      u <- from table
      where_ (u ^. UserEmail ==. val email)
      pure u

mkUserInfo :: Entity User -> UserInfo
mkUserInfo (Entity userID user) = UserInfo
  { userID = sqlKey2userID userID
  , userName  = user.userUsername
  , userPass  = user.userPassword & HashedPassword
  , userEmail = user.userEmail
  , userBio   = user.userBio
  , userImage = user.userImage
  }
