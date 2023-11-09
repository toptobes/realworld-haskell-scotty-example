{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.GetProfile where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB (runDB))
import Conduit.Features.Account.DB (Follow, User(..), userID2sqlKey)
import Conduit.Features.Account.Errors (AccountError(..), mapMaybeDBResult, withAccountErrorsHandled)
import Conduit.Features.Account.Types (InUserObj (InUserObj), UserID(..), UserProfile(..))
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), from, just, leftJoin, on, selectOne, table, val, where_, (&&.), (:&)(..), (==.), (?.), (^.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)

handleGetProfile :: ScottyT AppM ()
handleGetProfile = get "/api/profiles/:username" $ maybeWithAuth \user -> do
  userName <- captureParam "username"
  profile <- liftApp $ tryGetUser userName user
  withAccountErrorsHandled profile $
    json . InUserObj

tryGetUser :: (MonadIO m, AcquireUser m) => Text -> Maybe AuthedUser -> m (Either AccountError UserProfile)
tryGetUser userName currUser =
  let userID = currUser <&> authedUserID
   in fmap makeProfile <$> findUserByName userName userID

makeProfile :: UserInfo -> UserProfile
makeProfile user = UserProfile
  { userName     = user.userName
  , userBio      = user.userBio
  , userImage    = user.userImage
  , userFollowed = user.userFollowed
  }

class (Monad m) => AcquireUser m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userName     :: !Text
  , userBio      :: !(Maybe Text)
  , userImage    :: !(Maybe Text)
  , userFollowed :: !Bool
  }

instance (Monad m, MonadUnliftIO m, MonadIO m, MonadDB m) => AcquireUser m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError UserInfo)
  findUserByName name userID = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      (u :& f) <- from $ 
        table @User
          `leftJoin` 
        table @Follow
          `on` \(u :& f) ->
            just (u ^. #id) ==. f ?. #followerID &&. f ?. #followeeID ==. val (userID <&> userID2sqlKey)

      where_ (u ^. #username ==. val name)

      pure (u :& f)

mkUserInfo :: (Entity User :& Maybe (Entity Follow)) -> UserInfo
mkUserInfo ((Entity _ user) :& (isJust -> followed)) = UserInfo
  { userName  = user.userUsername
  , userBio   = user.userBio
  , userImage = user.userImage
  , userFollowed = followed
  }
