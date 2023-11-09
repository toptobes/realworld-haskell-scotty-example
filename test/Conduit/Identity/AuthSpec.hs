module Conduit.Identity.AuthSpec (spec) where

import Prelude hiding (exp)
import Conduit.Features.Account.Types (UserID(..))
import Conduit.Identity.Auth
import Conduit.Identity.JWT (JWTInfo(..), Seconds(..))
import Data.Time (NominalDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.JWT (EncodeSigner, VerifySigner, hmacSecret, toVerify, exp, numericDate)

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
  let commonUserID = UserID 3
      commonTime   = 0
      commonToken  = makeAuthTokenPure jwtInfo commonTime commonUserID

  describe "makeAuthTokenPure + tryGetSubjectFromJWT" do
    it "can decode an encoded subject" do
      let decodedUserID = tryGetSubjectFromJWT jwtInfo commonToken

      Just commonUserID == decodedUserID `shouldBe` True

    it "has proper exp time" do
      let claims = tryGetClaims jwtInfo commonToken
          expectedTime = commonTime + fromIntegral jwtInfo.jwtExpTime.unSeconds

      Just (numericDate expectedTime) == (claims <&> exp) `shouldBe` True

  describe "tryMakeAuthedUser" do
    it "works? idk lol" do
      let header = "Token " <> commonToken
          user = tryMakeAuthedUser jwtInfo header

      Just (AuthedUser commonToken commonUserID) == user `shouldBe` True
