{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Auth where

import Conduit.App.Has (Has, grab)
import Conduit.Features.Account.Types (UserID)
import Conduit.Identity.JWT (JWTInfo(..), jwtExpTime, mkClaims)
import Conduit.Utils ((-.))
import Data.Map.Strict as M
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Types (status401)
import Relude.Extra (dup)
import Web.JWT (JWTClaimsSet(..), claims, decodeAndVerifySignature, encodeSigned, numericDate, stringOrURIToText)
import Web.Scotty.Trans (ActionT, header, json, status)

-- | The form of an authenticated user passed into any endpoint using 'withAuth'/'maybeWithAuth'.
data AuthedUser = AuthedUser
  { authedToken  :: !Text
  , authedUserID :: !UserID
  } deriving (Eq, Show)

-- | An endpoint which requires user authentication.
-- 
-- > endpoint = get "/" $ withAuth \(user :: AuthedUser) -> do
-- >   ...
withAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c m) => (AuthedUser -> ActionT m ()) -> ActionT m ()
withAuth handler = maybeWithAuth $ \case
  Just user -> handler user
  Nothing -> status status401 >> json authErrRes

-- | An endpoint which requests, but does require, user authentication.
-- 
-- > endpoint = get "/" $ maybeWithAuth \(user :: Maybe AuthedUser) -> do
-- >   ...
maybeWithAuth :: (MonadIO m, Has JWTInfo c m) => (Maybe AuthedUser -> ActionT m ()) -> ActionT m ()
maybeWithAuth handler = do
  authHeader <- header "Authorization"
  jwtInfo <- lift $ grab @JWTInfo
  currTime <- liftIO getPOSIXTime
  handler $ authHeader >>= tryMakeAuthedUser jwtInfo currTime

-- | Some monad which can generate a JWT
class (Monad m) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text

instance (Monad m, MonadIO m, Has JWTInfo c m) => AuthTokenGen m where
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

authErrRes :: Map Text Text
authErrRes = M.fromList [("message", "missing authorization credentials")]
