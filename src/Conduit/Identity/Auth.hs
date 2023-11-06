{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Auth where

import Conduit.App.Has (Has (..), grab)
import Conduit.Features.Account.Types (UserID)
import Conduit.Identity.JWT (JWTInfo (..), jwtExpTime, mkClaims)
import Conduit.Utils ((-.))
import Data.Text
import Network.HTTP.Types (status403)
import Relude.Extra (dup)
import Web.JWT (VerifySigner, claims, decodeAndVerifySignature, encodeSigned, stringOrURIToText, sub)
import Web.Scotty.Trans (ActionT, header, status)

newtype AuthedJWT = AuthedJWT 
  { unJWT :: Text 
  }

data AuthedUser = AuthedUser
  { authedToken  :: !AuthedJWT
  , authedUserID :: !UserID
  }

withAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (AuthedUser -> ActionT m ()) -> ActionT m ()
withAuth handler = maybeWithAuth \case
  Just user -> handler user
  Nothing -> status status403

maybeWithAuth :: (MonadIO m, MonadReader c m, Has JWTInfo c) => (Maybe AuthedUser -> ActionT m ()) -> ActionT m ()
maybeWithAuth handler = do
  authHeader <- header "Authorization"
  JWTInfo {..} <- lift $ grab @JWTInfo
  handler $ tryGetSubjectFromJWT authHeader jwtVerifySigner

class (Monad m) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text

instance (Monad m, MonadIO m, MonadReader c m, Has JWTInfo c) => AuthTokenGen m where
  mkAuthToken :: UserID -> m Text
  mkAuthToken userID = do
    JWTInfo {..} <- grab @JWTInfo
    claims' <- liftIO $ mkClaims jwtExpTime userID
    pure $ encodeSigned jwtEncodeSigner mempty claims'

tryGetSubjectFromJWT :: (ToText a) => Maybe a -> VerifySigner -> Maybe AuthedUser
tryGetSubjectFromJWT authHeader verifySigner = authHeader 
  <&> toText
  >>= extractToken
  <&> dup
   -. bimap AuthedJWT (tryGetSubjectFromJWT' verifySigner)
  >>= sequence
  <&> uncurry AuthedUser

tryGetSubjectFromJWT' :: VerifySigner -> Text -> Maybe UserID
tryGetSubjectFromJWT' verifySigner token = token
   & decodeAndVerifySignature verifySigner
  <&> claims
  >>= sub
  <&> stringOrURIToText
   -. toString
  >>= readMaybe

extractToken :: Text -> Maybe Text
extractToken str = case splitOn " " str of
  ["Authorization:", "Token", token] -> Just token
  _ -> Nothing
