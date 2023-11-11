{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.GetProfile where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..), SqlKey(..))
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.Features.Account.DB (Follow, User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserID(..), UserProfile(..), inProfileObj)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), from, just, leftJoin, on, selectOne, table, val, where_, (&&.), (:&)(..), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)

handleGetProfile :: ScottyT AppM ()
handleGetProfile = get "/api/profiles/:username" $ maybeWithAuth \user -> do
  userName <- captureParam "username"
  profile <- liftApp $ getUserProfile userName user
  withFeatureErrorsHandled profile $
    json . inProfileObj

getUserProfile :: (AcquireProfile m) => Text -> Maybe AuthedUser -> m (Either AccountError UserProfile)
getUserProfile userName currUser =
  let userID = currUser <&> authedUserID
   in fmap makeProfile <$> findUserByName userName userID

makeProfile :: UserInfo -> UserProfile
makeProfile user = UserProfile
  { userName     = user.userName
  , userBio      = user.userBio
  , userImage    = user.userImage
  , userFollowed = user.userFollowed
  }

class (Monad m) => AcquireProfile m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userID       :: !UserID
  , userName     :: !Text
  , userBio      :: !(Maybe Text)
  , userImage    :: !(Maybe Text)
  , userFollowed :: !Bool
  }

-- couldn't figure out how to use `exists`, will try again later
instance (Monad m, MonadUnliftIO m, MonadIO m, MonadDB m) => AcquireProfile m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError UserInfo)
  findUserByName name userID = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      (u :& f) <- from $ 
        table @User
          `leftJoin` 
        table @Follow
          `on` \(u :& f) ->
            (just u.id ==. f.followerID) &&. (f.followeeID ==. val (userID <&> id2sqlKey))

      where_ (u.username ==. val name)

      pure (u, f)

mkUserInfo :: (Entity User, Maybe (Entity Follow)) -> UserInfo
mkUserInfo (Entity userID user, isJust -> followed) = UserInfo
  { userID    = sqlKey2ID userID
  , userName  = user.userUsername
  , userBio   = user.userBio
  , userImage = user.userImage
  , userFollowed = followed
  }
