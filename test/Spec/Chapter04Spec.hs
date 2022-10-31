module Spec.Chapter04Spec where

import Chapter04.Exercises
import Test.Hspec (describe, it, shouldBe)

spec =
  describe "Chapter 04" $ do
    describe "isPalindrome" $ do
      it "returns True" $ do
        isPalindrome "abba" `shouldBe` True
      it "returns False" $ do
        isPalindrome "Andrea" `shouldBe` False
