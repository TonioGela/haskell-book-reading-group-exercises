{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tests () where

import Data.Maybe (isJust, isNothing)
import Morra (Move (Move), Winner (First, Second, Tie), askMove, getWinner, mkMove, mkPlayed)
import System.Random.Stateful (randomIO)
import Test.Hspec
import Test.QuickCheck

testMkPlayed :: IO ()
testMkPlayed = hspec $ do
  describe "mkPlayed" $ do
    it "should return a Played value" $ do
      mkPlayed 0 `shouldBe` Just 0
      mkPlayed 1 `shouldBe` Just 1
      mkPlayed 2 `shouldBe` Just 2
      mkPlayed 3 `shouldBe` Just 3
      mkPlayed 4 `shouldBe` Just 4
      mkPlayed 5 `shouldBe` Just 5
    it "should return Nothing if the number is not between 1 and 5" $ do
      mkPlayed 6 `shouldBe` Nothing

testMkMove :: IO ()
testMkMove = hspec $ do
  describe "mkMove" $ do
    it "should return a Move value" $ do
      isJust (mkMove 0 0) `shouldBe` True
      isJust (mkMove 0 1) `shouldBe` True
      isJust (mkMove 0 2) `shouldBe` True
      isJust (mkMove 0 3) `shouldBe` True
      isJust (mkMove 0 4) `shouldBe` True
      isJust (mkMove 0 5) `shouldBe` True
    it "should return Nothing if the number is not between 1 and 5" $ do
      isNothing (mkMove 0 6) `shouldBe` True

testGetWinner :: IO ()
testGetWinner = hspec $ do
  describe "getWinner" $ do
    it "returns Tie when both players guess the sum correctly" $
      do
        getWinner (Move 3 1) (Move 3 2) `shouldBe` Tie

    it "returns First when only the first player guesses the sum correctly" $
      do
        getWinner (Move 3 1) (Move 2 2) `shouldBe` First

    it "returns Second when only the second player guesses the sum correctly" $
      do
        getWinner (Move 2 1) (Move 3 2) `shouldBe` Second

    it "returns Tie when no players guess the sum correctly" $
      do
        getWinner (Move 2 1) (Move 2 2) `shouldBe` Tie

main :: IO ()
main = do
  testMkPlayed
  testMkMove
  testGetWinner
  putStrLn "All tests passed!"
