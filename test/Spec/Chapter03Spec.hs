module Spec.Chapter03Spec where

import Chapter03.BuildingFunctions
import Test.Hspec (describe, it, shouldBe)

spec =
  describe "Chapter 03" $ do
    describe "building functions" $ do
      describe "fa" $ do
        it "append !" $ do
          appendExclamation "Curry is awesome" `shouldBe` "Curry is awesome!"
        it "append ! again" $ do
          appendExclamation "Andrea" `shouldBe` "Andrea!"
      describe "fb" $ do
        it "get 5th char" $ do
          get5th "Curry is awesome!" `shouldBe` 'y'
        it "get 5th char again" $ do
          get5th "Andrea" `shouldBe` 'e'
      describe "fc" $ do
        it "drop 9 chars" $ do
          drop9 "Curry is awesome!" `shouldBe` "awesome!"
        it "drop 9 chars again" $ do
          drop9 "Andrea" `shouldBe` ""
