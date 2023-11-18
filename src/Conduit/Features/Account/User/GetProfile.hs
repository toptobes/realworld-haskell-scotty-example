{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.User.GetProfile where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (mapMaybeDBResult, withFeatureErrorsHandled)
import Conduit.DB.Types (MonadDB(..), sqlKey2ID)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (mkProfile)
import Conduit.Features.Account.Errors (AccountError(..))
import Conduit.Features.Account.Exports.QueryUserFollows (queryIfUserFollows)
import Conduit.Features.Account.Types (UserID(..), UserProfile(..), inProfileObj)
import Conduit.Identity.Auth (AuthedUser(..), maybeWithAuth)
import Database.Esqueleto.Experimental (Entity(..), from, selectOne, table, val, (==.))
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
   in fmap snd <$> findUserByName userName userID

class (Monad m) => AcquireProfile m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError (UserID, UserProfile))

instance (Monad m, MonadUnliftIO m, MonadIO m, MonadDB m) => AcquireProfile m where
  findUserByName :: Text -> Maybe UserID -> m (Either AccountError (UserID, UserProfile))
  findUserByName name userID = mapMaybeDBResult UserNotFoundEx processUser <$> runDB do
    selectOne $ do
      u <- from table `suchThat` \u -> u.username ==. val name

      let follows = queryIfUserFollows u userID

      pure (u, follows)
    where 
      processUser = uncurry \e@(Entity key _) follows -> (sqlKey2ID key, mkProfile e follows)
