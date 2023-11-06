{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.GetUser where

import Prelude hiding (get)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.DB (usersTable)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserAuth (..), UserID (..))
import Conduit.Identity.Auth (AuthedUser (..), unJWT, withAuth)
import Database.Selda (MonadSelda, query, restrict, select, (!), (.==), (:*:) (..))
import Database.Selda qualified as S
import UnliftIO (stringException)
import UnliftIO.Exception (throwIO)
import Web.Scotty.Trans (ScottyT, get, json)

handleGetUser :: ScottyT AppM ()
handleGetUser = get "/" $ withAuth (liftApp . getUser >=> json)

getUser :: (AquireUser m) => AuthedUser -> m (InUserObj UserAuth)
getUser AuthedUser {..} = do
  user <- findUserById authedUserID

  pure $ InUserObj UserAuth
    { userToken = authedToken.unJWT
    , userName  = user.userName
    , userEmail = user.userEmail
    , userBio   = user.userBio
    , userImage = user.userImage
    }

class (Monad m) => AquireUser m where
  findUserById :: UserID -> m UserInfo

data UserInfo = UserInfo
  { userName  :: !Text
  , userEmail :: !Text
  , userBio   :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }

instance (Monad m, MonadSelda m) => AquireUser m where
  findUserById :: UserID -> m UserInfo
  findUserById (UserID userID) = do
    let userIDCol = S.literal $ S.toId userID

    extractUserInfo =<< query do
      u <- select usersTable
      restrict (u ! #user_id .== userIDCol)
      pure (u ! #username :*: u ! #email :*: u ! #bio :*: u ! #image)

extractUserInfo :: (MonadIO m) => [Text :*: Text :*: Maybe Text :*: Maybe Text] -> m UserInfo
extractUserInfo [] = liftIO . throwIO $ stringException "go away"
extractUserInfo [name :*: email :*: bio :*: image] = pure $ UserInfo name email bio image
extractUserInfo _ = error "should never happen b/c of unique email restriction"
