{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Follows.UnfollowUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Types (MonadDB(..))
import Conduit.DB.Errors (mapDBError, withFeatureErrorsHandled)
import Conduit.Features.Account.User.GetProfile (AcquireProfile(..), makeProfile, userID)
import Conduit.Features.Account.DB (Follow)
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Types (UserID(..), UserProfile(..), inProfileObj)
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (delete, from, table, valkey, where_, (&&.), (==.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json)
import Web.Scotty.Trans qualified as Scotty

handleUserUnfollow :: ScottyT AppM ()
handleUserUnfollow = Scotty.delete "/api/profiles/:username/follow" $ withAuth \followee -> do
  follower <- captureParam "username"
  profile <- liftApp (tryFollowUser follower followee.authedUserID)
  withFeatureErrorsHandled profile $
    json . inProfileObj

tryFollowUser :: (AcquireProfile m, DeleteFollow m) => Text -> UserID -> m (Either AccountError UserProfile)
tryFollowUser followerName followeeID = runExceptT do
  follower <- ExceptT $ findUserByName followerName (Just followeeID)
  ExceptT $ deleteFollow follower.userID followeeID
  pure (makeProfile follower) 
    { userFollowed = False
    }

class (Monad m) => DeleteFollow m where
  deleteFollow :: UserID -> UserID -> m (Either AccountError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => DeleteFollow m where
  deleteFollow :: UserID -> UserID -> m (Either AccountError ())
  deleteFollow follower followee = mapDBError <$> runDB do
    delete $ do
      f <- from $ table @Follow
      where_ $ (f.followeeID ==. valkey followee.unID) &&. (f.followerID ==. valkey follower.unID)
