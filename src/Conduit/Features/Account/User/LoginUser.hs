{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.LoginUser where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), mapMaybeDBResult, sqlKey2ID)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID, inUserObj)
import Conduit.Identity.Auth (AuthTokenGen (mkAuthToken))
import Conduit.Identity.Password (HashedPassword(..), UnsafePassword(..), testPassword)
import Conduit.Validation (NotBlank(..), parseJsonBody, (<!<))
import Data.Aeson (FromJSON(..), withObject, (.:))
import Database.Esqueleto.Experimental (Entity(..), from, selectOne, val, (==.))
import Database.Esqueleto.Experimental.From (table)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, json, post)

data LoginUserAction = LoginUserAction
  { password :: UnsafePassword
  , email    :: Text
  }

instance FromJSON LoginUserAction where
  parseJSON = withObject "LoginUserAction" $ \v -> LoginUserAction
    <$> v .: "password" <!< NotBlank
    <*> v .: "email"    <!< NotBlank

handleUserLogin :: ScottyT AppM ()
handleUserLogin = post "/api/users/login" do
  action <- parseJsonBody
  userAuth <- runService $ tryLoginUser action
  json $ inUserObj userAuth

tryLoginUser :: (MonadIO m, AcquireUser m, AuthTokenGen m) => LoginUserAction -> m (Either AccountError UserAuth)
tryLoginUser LoginUserAction {..} = runExceptT do
  user <- ExceptT $ findUserByEmail email

  let isValidPassword = testPassword password user.pass

  either' <- lift $ if isValidPassword
    then Right <$> createUserAuth user
    else pure . Left $ BadLoginCredsEx

  ExceptT $ pure either'

createUserAuth :: (AuthTokenGen m) => UserInfo -> m UserAuth
createUserAuth userInfo = do
  token <- mkAuthToken userInfo.userID

  pure UserAuth
    { token = token
    , email = userInfo.email
    , name  = userInfo.name
    , bio   = userInfo.bio
    , image = userInfo.image
    }

class (Monad m) => AcquireUser m where
  findUserByEmail :: Text -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userID    :: !UserID
  , name  :: !Text
  , email :: !Text
  , pass  :: !HashedPassword
  , bio   :: !(Maybe Text)
  , image :: !Text
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
  , name  = user.userUsername
  , pass  = user.userPassword & HashedPassword
  , email = user.userEmail
  , bio   = user.userBio
  , image = user.userImage
  }
