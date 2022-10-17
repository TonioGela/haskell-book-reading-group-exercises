module Spec.Chapter02Spec where

import Chapter02 (circleArea)
import Test.Hspec (describe, it, shouldBe)

spec =
  describe "Chapter 02" $ do
    it "calculates area of circle w/ radius 5" $ do
      circleArea 5 `shouldBe` pi * 5 * 5
    it "calculates area of circle w/ radius 10" $ do
      circleArea 10 `shouldBe` pi * 10 * 10
    it "calculates area of circle w/ radius 2" $ do
      circleArea 2 `shouldBe` pi * 2 * 2
    it "calculates area of circle w/ radius 4" $ do
      circleArea 4 `shouldBe` pi * 4 * 4
