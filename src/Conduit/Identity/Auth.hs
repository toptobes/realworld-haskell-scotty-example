{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Auth where

import Conduit.App.Has (Has (..), grab)
import Conduit.Features.Account.Types (UserID)
import Conduit.Identity.JWT (JWTInfo (..), jwtExpTime, mkClaims)
import Conduit.Utils ((-.))
import Data.Text
import Network.HTTP.Types (status403)
import Relude.Extra (dup)
import Web.JWT (claims, decodeAndVerifySignature, encodeSigned, stringOrURIToText, JWTClaimsSet(..), numericDate)
import Web.Scotty.Trans (ActionT, header, status)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

-- newtype AuthedJWT = AuthedJWT 
--   { unJWT :: Text 
--   }

data AuthedUser = AuthedUser
  { authedToken  :: !Text
  , authedUserID :: !UserID
  } deriving (Eq, Show)

withAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (AuthedUser -> ActionT m ()) -> ActionT m ()
withAuth handler = maybeWithAuth $ \case
  Just user -> handler user
  Nothing -> status status403

maybeWithAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (Maybe AuthedUser -> ActionT m ()) -> ActionT m ()
maybeWithAuth handler = do
  authHeader <- header "Authorization"
  jwtInfo <- lift $ grab @JWTInfo
  currTime <- liftIO getPOSIXTime
  handler $ authHeader >>= tryMakeAuthedUser jwtInfo currTime

class (Monad m) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text

instance (Monad m, MonadIO m, MonadReader c m, Has JWTInfo c) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text
  mkAuthToken userID = do
    jwtInfo <- grab @JWTInfo
    currTime <- liftIO getPOSIXTime
    pure $ makeAuthTokenPure jwtInfo currTime userID

makeAuthTokenPure :: JWTInfo -> POSIXTime -> UserID -> Text
makeAuthTokenPure JWTInfo {..} currTime userID =
  let claims' = mkClaims currTime jwtExpTime userID
   in encodeSigned jwtEncodeSigner mempty claims'

tryMakeAuthedUser :: (ToText a) => JWTInfo -> POSIXTime -> a -> Maybe AuthedUser
tryMakeAuthedUser jwtInfo time authHeader  = authHeader 
   &  toText
   &  extractToken
  <&> dup
   -. second (tryGetSubjectFromJWT jwtInfo time)
  >>= sequence
  <&> uncurry AuthedUser

tryGetSubjectFromJWT :: JWTInfo -> POSIXTime -> Text -> Maybe UserID
tryGetSubjectFromJWT jwtInfo time token = token
   &  tryGetClaims jwtInfo
  >>= \clms -> guard (clms.exp > numericDate time) $> clms
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
  ["Token", token] -> Just token
  _ -> Nothing