module Conduit.Identity.JWT where

import Conduit.Features.Account.Types (UserID, unUserID)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.JWT (EncodeSigner, JWTClaimsSet (..), VerifySigner, numericDate, stringOrURI, toVerify)

newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Show)

data JWTInfo = JWTInfo
  { jwtEncodeSigner :: !EncodeSigner
  , jwtVerifySigner :: !VerifySigner
  , jwtExpTime      :: !Seconds
  }

mkJWTInfo :: EncodeSigner -> Seconds -> JWTInfo
mkJWTInfo signer = JWTInfo signer (toVerify signer)

mkClaims :: Seconds -> UserID -> IO JWTClaimsSet
mkClaims ttl userID = do
  currentTime <- getPOSIXTime

  pure mempty
    { iss = stringOrURI "conduit-api"
    , aud = Left <$> stringOrURI "conduit-client"
    , sub = stringOrURI $ show userID.unUserID
    , exp = numericDate $ currentTime + fromIntegral ttl.unSeconds
    }
