{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.GetUser where

import Prelude hiding (get)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.Features.Account.Types (UserAuth(..), UserID)
import Conduit.Identity.Auth (AuthedUser(..), unJWT, withAuth)
import Web.Scotty.Trans (ScottyT, get, json)
import Conduit.App.Has (Has)
import Conduit.DB (DBPool)

handleGetUser :: ScottyT AppM ()
handleGetUser = get "/" $ withAuth (liftApp . getUser >=> json)

getUser :: (AquireUser m) => AuthedUser -> m UserAuth
getUser AuthedUser {..} = do
  user <- findUserById authedUserID

  pure UserAuth 
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

instance (Monad m, MonadReader c m, Has DBPool c) => AquireUser m where  
  findUserById :: UserID -> m UserInfo
  findUserById = undefined
