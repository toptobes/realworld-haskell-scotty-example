{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.LoginUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), sqlKey2ID)
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID, inUserObj)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), UnsafePassword(..), testPassword)
import Data.Aeson (FromJSON)
import Database.Esqueleto.Experimental (Entity(..), from, selectOne, val, (==.))
import Database.Esqueleto.Experimental.From (table)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post)
import Conduit.Validation (Validations, are, notBlank, fromJsonObj)
import Conduit.DB.Utils (suchThat)

data LoginUserAction = LoginUserAction
  { password :: UnsafePassword
  , email    :: Text
  } deriving (Generic, FromJSON)

validations :: Validations LoginUserAction
validations LoginUserAction {..} =
  [ (password.getUnsafe, "password")
  , (email,              "email")
  ] `are` notBlank

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  action <- fromJsonObj validations
  userAuth <- liftApp (tryLoginUser action)
  withFeatureErrorsHandled userAuth $
    json . inUserObj

tryLoginUser :: (MonadIO m, AcquireUser m, AuthTokenGen m) => LoginUserAction -> m (Either AccountError UserAuth)
tryLoginUser LoginUserAction {..} = runExceptT do
  user <- ExceptT $ findUserByEmail email

  let isValidPassword = testPassword password user.userPass

  either' <- lift $ if isValidPassword
    then Right <$> createUserAuth user
    else pure . Left $ BadLoginCredsEx

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
  , userImage :: !Text
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => AcquireUser m where
  findUserByEmail :: Text -> m (Either AccountError UserInfo)
  findUserByEmail email = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      from table `suchThat` \u ->
         u.email ==. val email

mkUserInfo :: Entity User -> UserInfo
mkUserInfo (Entity userID user) = UserInfo
  { userID = sqlKey2ID userID
  , userName  = user.userUsername
  , userPass  = user.userPassword & HashedPassword
  , userEmail = user.userEmail
  , userBio   = user.userBio
  , userImage = user.userImage
  }
