module Conduit.Identity.PasswordSpec (spec) where

import Conduit.Identity.Password
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "hashPassword + testPassword" do
    it "validates valid passwords" do
      let unsafe = UnsafePassword "password"

      hashed <- liftIO $ hashPassword unsafe

      testPassword unsafe hashed `shouldBe` True
      
    it "doesn't validate different passwords" do
      let unsafeReal = UnsafePassword "password"
          unsafeFake = UnsafePassword "fakepass"
    
      hashed <- liftIO $ hashPassword unsafeReal

      testPassword unsafeFake hashed `shouldBe` False
      
    it "produces unique hashes every attempt" do
      hashed1 <- liftIO . hashPassword $ UnsafePassword "password"
      hashed2 <- liftIO . hashPassword $ UnsafePassword "password"
      hashed1 == hashed2 `shouldBe` False
