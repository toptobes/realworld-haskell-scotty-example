{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Auth where

import Conduit.App.Has (Has (..), grab)
import Conduit.Features.Account.Types (UserID)
import Conduit.Identity.JWT (JWTInfo (..), jwtExpTime, mkClaims)
import Conduit.Utils ((-.))
import Data.Text
import Network.HTTP.Types (status403)
import Relude.Extra (dup)
import Web.JWT (claims, decodeAndVerifySignature, encodeSigned, stringOrURIToText, sub, JWTClaimsSet)
import Web.Scotty.Trans (ActionT, header, status)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time (NominalDiffTime)

-- newtype AuthedJWT = AuthedJWT 
--   { unJWT :: Text 
--   }

data AuthedUser = AuthedUser
  { authedToken  :: !Text
  , authedUserID :: !UserID
  } deriving (Eq, Show)

withAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (AuthedUser -> ActionT m ()) -> ActionT m ()
withAuth handler = maybeWithAuth \case
  Just user -> handler user
  Nothing -> status status403

maybeWithAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (Maybe AuthedUser -> ActionT m ()) -> ActionT m ()
maybeWithAuth handler = do
  authHeader <- header "Authorization"
  jwtInfo <- lift $ grab @JWTInfo
  handler $ authHeader >>= tryMakeAuthedUser jwtInfo

class (Monad m) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text

instance (Monad m, MonadIO m, MonadReader c m, Has JWTInfo c) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text
  mkAuthToken userID = do
    jwtInfo <- grab @JWTInfo
    currTime <- liftIO getPOSIXTime
    pure $ makeAuthTokenPure jwtInfo currTime userID

makeAuthTokenPure :: JWTInfo -> NominalDiffTime -> UserID -> Text
makeAuthTokenPure JWTInfo {..} currTime userID =
  let claims' = mkClaims currTime jwtExpTime userID
   in encodeSigned jwtEncodeSigner mempty claims'

tryMakeAuthedUser :: (ToText a) => JWTInfo -> a -> Maybe AuthedUser
tryMakeAuthedUser jwtInfo authHeader = authHeader 
   &  toText
   &  extractToken
  <&> dup
   -. second (tryGetSubjectFromJWT jwtInfo)
  >>= sequence
  <&> uncurry AuthedUser

tryGetSubjectFromJWT :: JWTInfo -> Text -> Maybe UserID
tryGetSubjectFromJWT jwtInfo token = token
   &  tryGetClaims jwtInfo
  >>= sub
  <&> stringOrURIToText
   -. toString
  >>= readMaybe

tryGetClaims :: JWTInfo -> Text -> Maybe JWTClaimsSet
tryGetClaims JWTInfo {..} token = token
   &  decodeAndVerifySignature jwtVerifySigner
  <&> claims

extractToken :: Text -> Maybe Text
extractToken str = case splitOn " " str of
  ["Authorization:", "Token", token] -> Just token
  _ -> Nothing
