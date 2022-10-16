module Spec.ItWorksSpec where

import ItWorks (hello)
import Test.Hspec (describe, it, shouldBe)

spec =
  describe "ItWorks" $ do
    it "has working tests" $ do
      hello `shouldBe` "Hello"