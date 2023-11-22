{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Follows.FollowUser where

import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB (..), id2sqlKey, mapDBError)
import Conduit.Features.Account.DB (Follow(..))
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Types (UserID, UserProfile(..), inProfileObj)
import Conduit.Features.Account.User.GetProfile (AcquireProfile(..))
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (insert_)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, post)

handleUserFollow :: ScottyT AppM ()
handleUserFollow = post "/api/profiles/:username/follow" $ withAuth \follower -> do
  followed <- captureParam "username"
  profile <- runService (tryFollowUser follower.authedUserID followed)
  json $ inProfileObj profile

tryFollowUser :: (AcquireProfile m, CreateFollow m) => UserID -> Text -> m (Either AccountError UserProfile)
tryFollowUser followerID followedName = runExceptT do
  (followedID, followedProfile) <- ExceptT $ findUserByName followedName (Just followerID)

  ExceptT $ addFollow followerID followedID

  pure followedProfile
    { userFollowed = True
    }

class (Monad m) => CreateFollow m where
  addFollow :: UserID -> UserID -> m (Either AccountError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateFollow m where
  addFollow :: UserID -> UserID -> m (Either AccountError ())
  addFollow (id2sqlKey -> follower) (id2sqlKey -> followed) = mapDBError <$> runDB do
    insert_ $ Follow follower followed
