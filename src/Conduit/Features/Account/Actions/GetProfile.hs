{-# LANGUAGE MonoLocalBinds, UndecidableInstances #-}

module Conduit.Features.Account.Actions.GetProfile where

import Prelude hiding (get)
import Conduit.App.Monad (AppM)
import Conduit.DB (catchErrorInto)
import Conduit.Features.Account.DB (followsTable, usersTable)
import Conduit.Features.Account.Errors (AccountError(..), withAccountErrorsHandled)
import Conduit.Features.Account.Types (InProfileObj (InProfileObj), UserID(..), UserProfile(..))
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Conduit.Utils ((-.))
import Database.Selda (MonadSelda, Set(..), from, query, select, suchThat, (!), (.==), (:*:)(..))
import Database.Selda qualified as S
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, captureParam, get, json)

handleGetProfile :: ScottyT AppM ()
handleGetProfile = get "/api/profiles/:username" $ maybeWithAuth \user -> do
  userName <- captureParam "username"
  profile <- tryGetUser userName user
  withAccountErrorsHandled profile $
    json . InProfileObj

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

instance (Monad m, MonadUnliftIO m, MonadSelda m) => AcquireUser m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError UserInfo)
  findUserByName name maybeUserID = runExceptT do
    let maybeUserIDCol = S.literal . S.toId . unUserID <$> maybeUserID
        nameCol = S.text name

    result <- ExceptT $ catchErrorInto SomeSQLError $ query do
      u <- select usersTable `suchThat` (! #username) -. (.== nameCol)

      let checkIfFollows userIDCol =
           let followers = #follower_id `from` select followsTable `suchThat` (! #followee_id) -. (.== userIDCol)
            in (u ! #user_id) `isIn` followers

      let isFollower = maybe S.false checkIfFollows maybeUserIDCol

      pure (u ! #username :*: u ! #bio :*: u ! #image :*: isFollower)

    ExceptT . pure $ extractUserInfo result

extractUserInfo :: [Text :*: Maybe Text :*: Maybe Text :*: Bool] -> Either AccountError UserInfo
extractUserInfo [] = Left UserNotFoundEx
extractUserInfo [name :*: bio :*: image :*: followed] = pure $ UserInfo name bio image followed
extractUserInfo _ = error "should never happen b/c of unique name restriction"

-- Might swap out the above for just raw SQL; I don't like how the generated SQL is looking. Smth like this?
-- SELECT
--   u.username,
--   u.bio,
--   u.image,
--   CASE
--     WHEN f.follower_id IS NOT NULL THEN TRUE
--     ELSE FALSE
--   END AS following
-- FROM
--   users u
--   LEFT JOIN follows f ON u.user_id = f.followee_id AND f.follower_id = 'querying_user_id'
-- WHERE
--   u.username = 'queried_user_name';
