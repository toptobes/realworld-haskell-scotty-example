{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Follows.UnfollowUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapDBError, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..))
import Conduit.Features.Account.DB (Follow)
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Types (UserID(..), UserProfile(..), inProfileObj)
import Conduit.Features.Account.User.GetProfile (AcquireProfile(..))
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (delete, from, table, valkey, where_, (&&.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json)
import Web.Scotty.Trans qualified as Scotty

handleUserUnfollow :: ScottyT AppM ()
handleUserUnfollow = Scotty.delete "/api/profiles/:username/follow" $ withAuth \follower -> do
  followed <- captureParam "username"
  profile <- liftApp (tryFollowUser follower.authedUserID followed)
  withFeatureErrorsHandled profile $
    json . inProfileObj

tryFollowUser :: (AcquireProfile m, DeleteFollow m) => UserID -> Text -> m (Either AccountError UserProfile)
tryFollowUser followerID followedName = runExceptT do
  (followedID, followedProfile) <- ExceptT $ findUserByName followedName (Just followerID)

  ExceptT $ deleteFollow followerID followedID

  pure followedProfile
    { userFollowed = False
    }

class (Monad m) => DeleteFollow m where
  deleteFollow :: UserID -> UserID -> m (Either AccountError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => DeleteFollow m where
  deleteFollow :: UserID -> UserID -> m (Either AccountError ())
  deleteFollow followed follower = mapDBError <$> runDB do
    delete $ do
      f <- from $ table @Follow
      where_ $ (f.followerID ==. valkey follower.unID) &&. (f.followedID ==. valkey followed.unID)
