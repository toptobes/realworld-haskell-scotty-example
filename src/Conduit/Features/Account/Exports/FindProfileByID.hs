{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Exports.FindProfileByID where

import Prelude hiding (get, on)
import Conduit.DB.Errors (FeatureErrorMapper(..), mapMaybeDBResult)
import Conduit.DB.Types (MonadDB(..), SqlKey (id2sqlKey))
import Conduit.Features.Account.DB (Follow, User(..))
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Types (UserID(..), UserProfile(..))
import Database.Esqueleto.Experimental (Entity(..), from, just, leftJoin, on, selectOne, table, val, valkey, where_, (&&.), (:&)(..), (==.), (?.), (^.))
import UnliftIO (MonadUnliftIO)

findUserProfileByID :: (FeatureErrorMapper AccountError e, AcquireProfile m) => UserID -> Maybe UserID -> m (Either e UserProfile)
findUserProfileByID user currUser = bimap mapFeatureError makeProfile <$> findUserByID user currUser

makeProfile :: UserInfo -> UserProfile
makeProfile user = UserProfile
  { userName     = user.userName
  , userBio      = user.userBio
  , userImage    = user.userImage
  , userFollowed = user.userFollowed
  }

class (Monad m) => AcquireProfile m where
  findUserByID :: UserID -> Maybe UserID -> m (Either AccountError UserInfo)

data UserInfo = UserInfo
  { userName     :: !Text
  , userBio      :: !(Maybe Text)
  , userImage    :: !(Maybe Text)
  , userFollowed :: !Bool
  }

instance (Monad m, MonadUnliftIO m, MonadDB m) => AcquireProfile m where
  findUserByID :: UserID -> Maybe UserID -> m (Either AccountError UserInfo)
  findUserByID user currUser = mapMaybeDBResult UserNotFoundEx mkUserInfo <$> runDB do
    selectOne $ do
      (u :& f) <- from $ 
        table @User
          `leftJoin` 
        table @Follow
          `on` \(u :& f) ->
            just (u ^. #id) ==. f ?. #followerID &&. f ?. #followeeID ==. val (currUser <&> id2sqlKey)

      where_ (u ^. #id ==. valkey user.unID)

      pure (u :& f)

mkUserInfo :: (Entity User :& Maybe (Entity Follow)) -> UserInfo
mkUserInfo ((Entity _ user) :& (isJust -> followed)) = UserInfo
  { userName  = user.userUsername
  , userBio   = user.userBio
  , userImage = user.userImage
  , userFollowed = followed
  }
