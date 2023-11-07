{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.GetUser where

import Prelude hiding (get)
import Conduit.App.Monad (AppM)
import Conduit.DB (catchErrorInto)
import Conduit.Features.Account.DB (usersTable)
import Conduit.Features.Account.Errors (AccountError(..), withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth(..), UserID(..))
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Selda (MonadSelda, query, restrict, select, (!), (.==), (:*:)(..))
import Database.Selda qualified as S
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, get, json)

handleGetUser :: ScottyT AppM ()
handleGetUser = get "/api/user" $ withAuth \user -> do
  userAuth <- tryGetUser user
  withAccountErrorsHandled userAuth $
    json . InUserObj

tryGetUser :: (AcquireUser m) => AuthedUser -> m (Either AccountError UserAuth)
tryGetUser AuthedUser {..} = do
  maybeUserInfo <- findUserById authedUserID
  pure $ getUser authedToken <$> maybeUserInfo

getUser :: Text -> UserInfo -> UserAuth
getUser token user = UserAuth
  { userToken = token
  , userName  = user.userName
  , userEmail = user.userEmail
  , userBio   = user.userBio
  , userImage = user.userImage
  }

class (Monad m) => AcquireUser m where
  findUserById :: UserID -> m (Either AccountError  UserInfo)

data UserInfo = UserInfo
  { userName  :: !Text
  , userEmail :: !Text
  , userBio   :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }

instance (Monad m, MonadUnliftIO m, MonadSelda m) => AcquireUser m where
  findUserById :: UserID -> m (Either AccountError  UserInfo)
  findUserById userID = runExceptT do
    let userIDCol = S.literal $ S.toId userID.unUserID

    result <- ExceptT $ catchErrorInto SomeSQLError $ query do
      u <- select usersTable
      restrict (u ! #user_id .== userIDCol)
      pure (u ! #username :*: u ! #email :*: u ! #bio :*: u ! #image)

    ExceptT . pure $ extractUserInfo result

extractUserInfo :: [Text :*: Text :*: Maybe Text :*: Maybe Text] -> Either AccountError UserInfo
extractUserInfo [] = Left UserNotFoundEx
extractUserInfo [name :*: email :*: bio :*: image] = pure $ UserInfo name email bio image
extractUserInfo _ = error "should never happen b/c of unique user_id restriction"
