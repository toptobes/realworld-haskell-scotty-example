{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Actions.FollowUser where

import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB (MonadDB(..))
import Conduit.Features.Account.Actions.GetProfile (AcquireUser(..), makeProfile, userID)
import Conduit.Features.Account.DB (Follow(..), userID2sqlKey)
import Conduit.Features.Account.Errors (AccountError, mapDBResult, withAccountErrorsHandled)
import Conduit.Features.Account.Types (InProfileObj(..), UserID, UserProfile(..))
import Conduit.Identity.Auth (AuthedUser(..), withAuth)
import Database.Esqueleto.Experimental (insert_)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, json, post)

handleUserFollow :: ScottyT AppM ()
handleUserFollow = post "/api/profiles/:username/follow" $ withAuth \followee -> do
  follower <- captureParam "username"
  profile <- liftApp (tryFollowUser follower followee.authedUserID)
  withAccountErrorsHandled profile $
    json . InProfileObj

tryFollowUser :: (AcquireUser m, CreateFollow m) => Text -> UserID -> m (Either AccountError UserProfile)
tryFollowUser followerName followeeID = runExceptT do
  follower <- ExceptT $ findUserByName followerName (Just followeeID)
  ExceptT $ addFollow follower.userID followeeID
  pure (makeProfile follower) 
    { userFollowed = True
    }

class (Monad m) => CreateFollow m where
  addFollow :: UserID -> UserID -> m (Either AccountError ())

instance (Monad m, MonadDB m, MonadUnliftIO m) => CreateFollow m where
  addFollow :: UserID -> UserID -> m (Either AccountError ())
  addFollow (userID2sqlKey -> follower) (userID2sqlKey -> followee) = mapDBResult id <$> runDB do 
    insert_ $ Follow follower followee
