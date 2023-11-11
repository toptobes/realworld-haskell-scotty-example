module Conduit.Features.Articles.SlugsSpec where

import Conduit.Features.Articles.Slugs
import Conduit.Features.Articles.Types
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "mkNoIDSlug" do
    it "correctly slugifys a string" do
      let slug = mkNoIDSlug "How to train your Dragon?!?!"
          expected = NoIDSlug "how-to-train-your-dragon"

      slug `shouldBe` expected
  
  describe "mkSlug" do
    it "correctly adds an ID to a slug" do
      let slug = mkSlug (ArticleID 3) (mkNoIDSlug "How to train your Dragon?!?!")
          expected = Slug "3-how-to-train-your-dragon"

      slug `shouldBe` expected
  
  describe "extractIDFromSlug" do
    it "gets the id from a slug" do
      let articleID = extractIDFromSlug $ mkSlug (ArticleID 3) (mkNoIDSlug "How to train your Dragon?!?!")
          expected = pure (ArticleID 3)

      articleID `shouldBe` expected
