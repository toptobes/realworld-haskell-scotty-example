{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Follows.UnfollowUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB(..))
import Conduit.Errors (FeatureErrorHandler(..), mapDBError)
import Conduit.Features.Account.User.GetProfile (AcquireUser(..), makeProfile, userID)
import Conduit.Features.Account.DB (EntityField(..))
import Conduit.Features.Account.Errors (AccountError)
import Conduit.Features.Account.Types (UserID(..), UserProfile(..), inProfileObj)
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (delete, from, table, valkey, where_, (&&.), (==.), (^.))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json)
import Web.Scotty.Trans qualified as Scotty

handleUserUnfollow :: ScottyT AppM ()
handleUserUnfollow = Scotty.delete "/api/profiles/:username/follow" $ withAuth \followee -> do
  follower <- captureParam "username"
  profile <- liftApp (tryFollowUser follower followee.authedUserID)
  withFeatureErrorsHandled profile $
    json . inProfileObj

tryFollowUser :: (AcquireUser m, DeleteFollow m) => Text -> UserID -> m (Either AccountError UserProfile)
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
      f <- from table
      where_ $ (f ^. FollowFolloweeID ==. valkey followee.unID) &&. (f ^. FollowFollowerID ==. valkey follower.unID)
