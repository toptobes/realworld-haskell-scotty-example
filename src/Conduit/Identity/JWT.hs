module Conduit.Identity.JWT where

import Conduit.Features.Account.Types (UserID, unUserID)
import Web.JWT (EncodeSigner, JWTClaimsSet (..), VerifySigner, numericDate, stringOrURI, toVerify)
import Data.Time (NominalDiffTime)

newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Show)
  deriving newtype (Num)

data JWTInfo = JWTInfo
  { jwtEncodeSigner :: !EncodeSigner
  , jwtVerifySigner :: !VerifySigner
  , jwtExpTime      :: !Seconds
  }

mkJWTInfo :: EncodeSigner -> Seconds -> JWTInfo
mkJWTInfo signer = JWTInfo signer (toVerify signer)

mkClaims :: NominalDiffTime -> Seconds -> UserID -> JWTClaimsSet
mkClaims currTime (Seconds ttl) userID = mempty
  { iss = stringOrURI "conduit-api"
  , aud = Left <$> stringOrURI "conduit-client"
  , sub = stringOrURI $ show userID.unUserID
  , exp = numericDate $ currTime + fromIntegral ttl
  }
