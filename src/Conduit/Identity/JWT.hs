module Conduit.Identity.JWT where

import Conduit.Features.Account.Types (UserID, unID)
import Data.Aeson (FromJSON)
import Data.Time (NominalDiffTime)
import Web.JWT (EncodeSigner, JWTClaimsSet(..), VerifySigner, hmacSecret, numericDate, stringOrURI, toVerify)

newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Show)
  deriving newtype (Num, FromJSON)

data JWTInfo = JWTInfo
  { jwtEncodeSigner :: !EncodeSigner
  , jwtVerifySigner :: !VerifySigner
  , jwtExpTime      :: !Seconds
  }

data JWTOps = JWTOps
  { jwtOpsSecret  :: Text
  , jwtOpsExpTime :: Seconds
  }

mkJWTInfo :: JWTOps -> JWTInfo
mkJWTInfo JWTOps {..} = JWTInfo signer (toVerify signer) jwtOpsExpTime
  where signer = hmacSecret jwtOpsSecret

mkClaims :: NominalDiffTime -> Seconds -> UserID -> JWTClaimsSet
mkClaims currTime (Seconds ttl) userID = mempty
  { iss = stringOrURI "conduit-api"
  , aud = Left <$> stringOrURI "conduit-client"
  , sub = stringOrURI $ show userID.unID
  , exp = numericDate $ currTime + fromIntegral ttl
  }
