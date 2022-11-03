import Test.Hspec
import Chapter04
import Chapter04 (isPalindrome, myAbs)

main :: IO ()
main = hspec $ do
  describe "length 1" $ do
    it "signature => length :: [a] -> Int" $ do
      myLength [1,2,3] `shouldBe` 3
    

  describe "length 2" $ do
    it "length [1,2,3,4,5] == 5" $ do
      length [1,2,3,4,5] `shouldBe` 5
    it "length [(1,2), (2,3), (3,4)] == 3" $ do
      length [(1,2), (2,3), (3,4)] `shouldBe` 3
    it "length allAwesome == 2" $ do
      length allAwesome `shouldBe` 2
    it "length (concat allAwesome) == 5" $ do
      length (concat allAwesome) `shouldBe` 5

  describe "length 4" $ do
    it "6 / 3 is correct" $ do
      6 / 3 `shouldBe` 2
    it "6 / (length [1,2,3]) is wrong, should use div" $ do
      6 `div` length [1,2,3] `shouldBe` 2

  describe "lenght 5" $ do
    it "2 + 3 == 5 evaluates to Bool" $ do
      (2 + 3 == 5) `shouldBe` True

  describe "length 6" $ do
    it "let x = 5; x + 3 == 5 evaluates to False" $ do
      let x = 5 in 
        (x + 3 == 5) `shouldBe` False

  describe "length 7" $ do
    it "length allAwesome == 2 reduces to True" $ do
      length allAwesome == 2 `shouldBe` True
    it "length [1, 'a', 3, 'b'] doesn't compile due to missing Num instance for Char" $
      True
    it "length allAwesome + length awesome reduces to 5" $ do
      (length allAwesome + length awesome) `shouldBe` 5
    it "(8 == 8) && ('b' < 'a') reduces to False" $ do
      ((8 == 8) && ('b' < 'a')) `shouldBe` False
    it "(8 == 8) && 9 doens't compile due to missing Num instance for Bool"
      True

  describe "length 8" $ do
    it "isPalindrome \"abba\" == True" $ do
      isPalindrome "abba" `shouldBe` True
    it "isPalindrome \"baba\" == False" $ do
      isPalindrome "baba" `shouldBe` False
  
  describe "length 9" $ do
    it "myAbs 5 == 5" $ do
      myAbs 5 `shouldBe` 5
    it "myAbs -5 == 5" $ do
      myAbs (-5) `shouldBe` 5
    it "myAbs 0 == 0" $ do
      myAbs 0 `shouldBe` 0

  describe "length 10" $ do
    it "f (1,'a') (True, 2.0) == (('a', 2.0), (1, True))" $ do
      f (1,'a') (True, 2.0) `shouldBe` (('a', 2.0), (1, True))

    it "f' (1,'a') (True, 2.0) == (('a', 2.0), (1, True))" $ do
      f' (1,'a') (True, 2.0) `shouldBe` (('a', 2.0), (1, True))

  
  describe "correcting syntaxt" $ do
    it "1. functions need to start with a lowercase letter" $ do
        let 
          f xs = w `x` 1 where
            w = length xs
            x = (+) in 
              f [1,2,3] `shouldBe` 4
    it "2. variables need to start with a lowercase letter, lambdas use ->" $ do
      map (\x -> x) [1,2,3] `shouldBe`  [1,2,3]

    it "3. tuple decomposition needs a ',' and variables are case sensitive" $ do
      let 
        f (a, b) = a in
          f (1, 2) `shouldBe` 1


        
    
  




