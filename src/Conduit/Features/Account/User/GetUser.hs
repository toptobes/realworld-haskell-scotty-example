{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.GetUser where

import Prelude hiding (get)
import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), mapMaybeDBResult)
import Conduit.Features.Account.DB (User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserAuth(..), UserID(..), inUserObj)
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Data.Aeson (ToJSON)
import Database.Esqueleto.Experimental (Entity(..), from, selectOne, table, valkey, where_, (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, get, json)

handleGetUser :: ScottyT AppM ()
handleGetUser = get "/api/user" $ withAuth \user -> do
  userAuth <- runService $ tryGetUser user
  json $ inUserObj userAuth

tryGetUser :: (AcquireUser m) => AuthedUser -> m (Either AccountError UserAuth)
tryGetUser AuthedUser {..} = do
  maybeUserInfo <- findUserById authedUserID
  pure $ mkUser authedToken <$> maybeUserInfo

mkUser :: Text -> UserInfo -> UserAuth
mkUser token user = UserAuth
  { userToken = token
  , userName  = user.userName
  , userEmail = user.userEmail
  , userBio   = user.userBio
  , userImage = user.userImage
  }

class (Monad m) => AcquireUser m where
  findUserById :: UserID -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userName  :: !Text
  , userEmail :: !Text
  , userBio   :: !(Maybe Text)
  , userImage :: Text
  } deriving (Generic, ToJSON)

instance (Monad m, MonadUnliftIO m, MonadDB m) => AcquireUser m where
  findUserById :: UserID -> m (Either AccountError UserInfo)
  findUserById userID = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      u <- from table
      where_ (u.id ==. valkey userID.unID)
      pure u

mkUserInfo :: Entity User -> UserInfo
mkUserInfo (Entity _ user) = UserInfo
  { userName  = user.userUsername
  , userEmail = user.userEmail
  , userBio   = user.userBio
  , userImage = user.userImage
  }
