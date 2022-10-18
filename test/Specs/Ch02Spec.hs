module Specs.Ch02Spec (spec) where

import Ch02 (half, piCalc1, piCalc2, square)
import Test.Hspec

spec :: Spec
spec = do
  describe "2.5 Evaluation" $ do
    describe "#half" $ do
      it "half 10" $ do
        half 10 `shouldBe` 5
      it "half 3" $ do
        half 3 `shouldBe` 1.5

    describe "#square" $ do
      it "square 10" $ do
        square 10 `shouldBe` 100
      it "square 3" $ do
        square 3 `shouldBe` 9

    describe "#piCalc1" $ do
      it "3.14 * (5 * 5)" $ do
        piCalc1 5 `shouldBe` 3.14 * (5 * 5)
      it "3.14 * (10 * 10)" $ do
        piCalc1 10 `shouldBe` 3.14 * (10 * 10)
      it "3.14 * (2 * 2)" $ do
        piCalc1 2 `shouldBe` 3.14 * (2 * 2)
      it "3.14 * (4 * 4)" $ do
        piCalc1 4 `shouldBe` 3.14 * (4 * 4)

    describe "#piCalc2" $ do
      it "3.14 * (5 * 5)" $ do
        piCalc2 5 `shouldBe` pi * (5 * 5)
      it "3.14 * (10 * 10)" $ do
        piCalc2 10 `shouldBe` pi * (10 * 10)
      it "3.14 * (2 * 2)" $ do
        piCalc2 2 `shouldBe` pi * (2 * 2)
      it "3.14 * (4 * 4)" $ do
        piCalc2 4 `shouldBe` pi * (4 * 4)

  describe "2.6 Infix operators" $ do
    it "1." $ do
      8 + 7 * 9 `shouldNotBe` (8 + 7) * 9
    it "2." $ do
      let x = 1
      let y = 2
      (x * 2) + (y * 2) `shouldBe` x * 2 + y * 2
    it "3." $ do
      let x = 1
      x / 2 + 9 `shouldNotBe` x / (2 + 9)