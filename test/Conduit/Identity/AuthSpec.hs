module Conduit.Identity.AuthSpec (spec) where

import Prelude hiding (exp)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Identity.Auth
import Conduit.Identity.JWT
import Data.Time (NominalDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.JWT (EncodeSigner, VerifySigner, exp, hmacSecret, numericDate, toVerify)

encodeSigner :: EncodeSigner
encodeSigner = hmacSecret "test-key"

verifySigner :: VerifySigner
verifySigner = toVerify encodeSigner

jwtInfo :: JWTInfo
jwtInfo = JWTInfo
  { jwtEncodeSigner = encodeSigner
  , jwtVerifySigner = verifySigner
  , jwtExpTime = Seconds 100
  }

spec :: Spec
spec = do
  let commonUserID  = UserID 3
      commonToken   = makeAuthTokenPure jwtInfo commonTime commonUserID
      commonTime    = 0 :: NominalDiffTime
      commonHeader  = "Token " <> commonToken
      commonExpTime = commonTime + fromIntegral jwtInfo.jwtExpTime.unSeconds

  describe "makeAuthTokenPure + tryGetSubjectFromJWT" do
    it "can decode an encoded subject" do
      let decodedUserID = tryGetSubjectFromJWT jwtInfo commonTime commonToken
      decodedUserID `shouldBe` Just commonUserID

    it "has proper exp time" do
      let claims = tryGetClaims jwtInfo commonToken
      (claims <&> exp) `shouldBe` Just (numericDate commonExpTime)

  describe "tryMakeAuthedUser" do
    it "works? idk lol" do
      let user = tryMakeAuthedUser jwtInfo 0 commonHeader
      user `shouldBe` Just (AuthedUser commonToken commonUserID)

    it "rejects old tookens" do
      let user = tryMakeAuthedUser jwtInfo commonExpTime commonHeader
      user `shouldBe` Nothing
