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
  userAuth <- runService $ getUser user
  json $ inUserObj userAuth

getUser :: (AcquireUser m) => AuthedUser -> m (Either AccountError UserAuth)
getUser AuthedUser {..} = do
  maybeUserInfo <- findUserById authedUserID
  pure $ mkUser authedToken <$> maybeUserInfo

mkUser :: Text -> UserInfo -> UserAuth
mkUser token user = UserAuth
  { token = token
  , name  = user.name
  , email = user.email
  , bio   = user.bio
  , image = user.image
  }

class (Monad m) => AcquireUser m where
  findUserById :: UserID -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { name  :: Text
  , email :: Text
  , bio   :: Maybe Text
  , image :: Text
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
  { name  = user.userUsername
  , email = user.userEmail
  , bio   = user.userBio
  , image = user.userImage
  }
