{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Common.FindProfileByID where

import Prelude hiding (get, on)
import Conduit.DB.Core (MonadDB(..), mapMaybeDBResult)
import Conduit.DB.Utils (suchThat)
import Conduit.Errors (FeatureErrorMapper(..))
import Conduit.Features.Account.Common.QueryUserFollows (queryIfUserFollows)
import Conduit.Features.Account.DB (mkProfile)
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserID(..), UserProfile(..))
import Database.Esqueleto.Experimental (from, selectOne, table, valkey, (==.))
import UnliftIO (MonadUnliftIO)

findUserProfileByID :: (FeatureErrorMapper AccountError e, AcquireProfile m) => UserID -> Maybe UserID -> m (Either e UserProfile)
findUserProfileByID user currUser = first mapFeatureError <$> findUserByID user currUser

class (Monad m) => AcquireProfile m where
  findUserByID :: UserID -> Maybe UserID -> m (Either AccountError UserProfile)

instance (Monad m, MonadUnliftIO m, MonadDB m) => AcquireProfile m where
  findUserByID :: UserID -> Maybe UserID -> m (Either AccountError UserProfile)
  findUserByID user currUserID = mapMaybeDBResult UserNotFoundEx (uncurry mkProfile) <$> runDB do
    selectOne $ do
      u <- from table `suchThat` \u -> u.id ==. valkey user.unID

      let follows = queryIfUserFollows u currUserID

      pure (u, follows)
