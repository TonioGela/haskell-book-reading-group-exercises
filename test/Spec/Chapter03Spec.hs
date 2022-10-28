module Spec.Chapter03Spec where

import Chapter03.BuildingFunctions
import Test.Hspec (describe, it, shouldBe)

spec =
  describe "Chapter 03" $ do
    describe "building functions" $ do
      describe "appendExclamation" $ do
        it "appends !" $ do
          appendExclamation "Curry is awesome" `shouldBe` "Curry is awesome!"
        it "appends ! again" $ do
          appendExclamation "Andrea" `shouldBe` "Andrea!"
      describe "get5th" $ do
        it "gets 5th char" $ do
          get5th "Curry is awesome!" `shouldBe` 'y'
        it "gets 5th char again" $ do
          get5th "Andrea" `shouldBe` 'e'
      describe "drop9" $ do
        it "drops 9 chars" $ do
          drop9 "Curry is awesome!" `shouldBe` "awesome!"
        it "drops 9 chars again" $ do
          drop9 "Andrea" `shouldBe` ""
      describe "thirdLetter" $ do
        it "gets 3rd char" $ do
          thirdLetter "Curry is awesome!" `shouldBe` 'r'
      describe "letterIndex" $ do
        it "gets letter at given index" $ do
          letterIndex 2 `shouldBe` 'r'
        it "gets letter at another index" $ do
          letterIndex 9 `shouldBe` 'a'
