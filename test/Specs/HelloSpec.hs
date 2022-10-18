module Specs.HelloSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Examples" $ do
    it "true == true" $ do
      True `shouldBe` True