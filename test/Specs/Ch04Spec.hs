module Specs.Ch04Spec (spec) where


import Test.Hspec
import Ch04 (allAwesome, awesome, isPalindrome, isPalindromeString, myAbs, f);
import Data.Typeable ( typeOf )

spec :: Spec
spec = do
  describe "4.9" $ do
    describe "2. length" $ do
      it "length for [1, 2, 3, 4, 5]" $ do 
        length [1, 2, 3, 4, 5] `shouldBe` 5
      it "length for [(1, 2), (2, 3), (3, 4)]" $ do 
        length [(1, 2), (2, 3), (3, 4)] `shouldBe` 3
      it "length for allAwesome" $ do 
        length allAwesome `shouldBe` 2
      it "length for (concat allAwesome)" $ do 
        length (concat allAwesome) `shouldBe` 5

    describe "3,4. length" $ do
      it "6/3 should works" $ do 
        6/3 `shouldBe` 2
      it "6 / length [1, 2, 3] should not works (fix)" $ do 
        6 `div` length [1, 2, 3] `shouldBe` 2

    describe "5, 6. bool" $ do
      it "2 + 3 == 5" $ do 
        2 + 3 == 5 `shouldBe` True
      it "type for x = 5; x + 3" $ do 
        let x = 5
        x + 3 == 5 `shouldBe` False

    describe "7." $ do
      it "length allAwesome == 2" $ do 
        length allAwesome == 2 `shouldBe` True
      it "length [1, 'a', 3, 'b']" $ do 
        -- doens't work since the list contains items of different types
        True
      it "length allAwesome + length awesome" $ do 
        length allAwesome + length awesome `shouldBe` 5
      it "(8 == 8) && ('b' < 'a')" $ do 
        (8 == 8) && ('b' < 'a') `shouldBe` False
      it "(8 == 8) && 9" $ do 
        -- doens't work since 9 is not Boolean
       True
    describe "8. isPalindrome" $ do
      it "anna" $ do
        isPalindrome "anna" `shouldBe` True
      it "ayeye" $ do
        isPalindrome "ayeye" `shouldBe` False
      it "with spaces" $ do
        isPalindromeString "o mordo tua nuora o aro un autodromo" `shouldBe` True

    describe "9. myAbs" $ do
      it "1" $ do
        myAbs 1 `shouldBe` 1  
      it "-1" $ do
        myAbs (-1) `shouldBe` 1  
      it "0" $ do
        myAbs 0 `shouldBe` 0
      it "-10" $ do
        myAbs (-10) `shouldBe` 10  

    describe "10. f tuples" $ do
      it "(1, 2) (3, 4)" $ do
        f (1, 2) (3, 4) `shouldBe` ((2, 4), (1, 3))