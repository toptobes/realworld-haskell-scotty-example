module Conduit.Identity.JWT where

import Conduit.Features.Account.Types (UserID, unID)
import Data.Aeson (FromJSON)
import Data.Time (NominalDiffTime)
import Web.JWT (EncodeSigner, JWTClaimsSet(..), VerifySigner, hmacSecret, numericDate, stringOrURI, toVerify)

-- | A datatype enforcing units be in Seconds.
newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Show)
  deriving newtype (Num, FromJSON)

-- | Application JWT config state.
data JWTInfo = JWTInfo
  { jwtEncodeSigner :: !EncodeSigner
  , jwtVerifySigner :: !VerifySigner
  , jwtExpTime      :: !Seconds
  }

-- | Initial JWT config state @ application startup, converted to 'JWTInfo'.
data JWTOps = JWTOps
  { jwtOpsSecret  :: !Text
  , jwtOpsExpTime :: !Seconds
  }

-- | Creates an 'JWTInfo' instance from 'JWTOps'.
mkJWTInfo :: JWTOps -> JWTInfo
mkJWTInfo JWTOps {..} = JWTInfo signer (toVerify signer) jwtOpsExpTime
  where signer = hmacSecret jwtOpsSecret

-- | Populates the desired claims:
--
--   [iss] Issuer of the JWT
--
--   [aud] Audience for the JWT
--
--   [sub] Subject of the JWT; a UserID
--
--   [exp] Expiration time of the JWT
mkClaims :: NominalDiffTime -> Seconds -> UserID -> JWTClaimsSet
mkClaims currTime (Seconds ttl) userID = mempty
  { iss = stringOrURI "conduit-api"
  , aud = Left <$> stringOrURI "conduit-client"
  , sub = stringOrURI $ show userID.unID
  , exp = numericDate $ currTime + fromIntegral ttl
  }
