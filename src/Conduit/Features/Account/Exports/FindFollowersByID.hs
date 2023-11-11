{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Exports.FindFollowersByID where

import Prelude hiding (get, on)
import Conduit.DB.Errors (FeatureErrorMapper(..), mapDBResult)
import Conduit.DB.Types (MonadDB(..), SqlKey (sqlKey2ID))
import Conduit.Features.Account.DB (Follow, UserId)
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserID(..))
import Database.Esqueleto.Experimental (from, table, valkey, where_, (==.), select, Value(..))
import UnliftIO (MonadUnliftIO)

findFollowersByID :: (FeatureErrorMapper AccountError e, AquireFollowers m) => UserID -> m (Either e [UserID])
findFollowersByID user = first mapFeatureError <$> findUserByID user

class (Monad m) => AquireFollowers m where
  findUserByID :: UserID -> m (Either AccountError [UserID])

data UserInfo = UserInfo
  { userName     :: !Text
  , userBio      :: !(Maybe Text)
  , userImage    :: !(Maybe Text)
  , userFollowed :: !Bool
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => AquireFollowers m where
  findUserByID :: UserID -> m (Either AccountError [UserID])
  findUserByID userID = mapDBResult toUserIDs <$> runDB do
    select $ do
      f <- from $ table @Follow
      where_ (f.followerID ==. valkey userID.unID)
      pure f.followeeID

toUserIDs :: [Value UserId] -> [UserID]
toUserIDs = map (\(Value userID) -> sqlKey2ID userID)
