import Test.Hspec
import Chapter02

main :: IO ()
main = hspec $ do
  describe "Parenthesization" $ do
    it "1. 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1" $ do
      (2 + 2 * 3 - 1) `shouldBe` (2 + (2 * 3) - 1)
    it "2. (^) 10 $ 1 + 1 == 10 ^ (1 + 1)" $ do
      ((^) 10 $ 1 + 1) `shouldBe` 10 ^ (1 + 1)
    it "3. 2 ^ 2 * 4 ^ 5 + 1 == (2 ^ 2) * (4 ^ 5) + 1" $ do
      (2 ^ 2 * 4 ^ 5 + 1) `shouldBe` ((2 ^ 2) * (4 ^ 5) + 1)

  describe "Equivalent expressions" $ do
    it "1 + 1 is equivalent to 2" $ do
      (1 + 1) `shouldBe` 2
    it "10 ^ 2 is equivalent to 10 + 9 * 10" $ do
      (10 ^ 2) `shouldBe` (10 + 9 * 10)
    it "400 - 37 is not equivalent to (-) 37 400" $ do
      (400 - 37) `shouldNotBe` ((-) 37 400)
    it "100 `div` 3 is not equivalent to 100 / 3" $ do
      (100 `div` 3) * 100 `shouldBe` 3300
      truncate ((100 / 3) * 100) `shouldBe` 3333
    it "2 * 5 + 18 is not equivalent 2 * (5 + 18)" $ do
      (2 * 5 + 18) `shouldNotBe` (2 * (5 + 18))

  describe "More fun with function" $ do
    it "10 + waxOn == 1135" $ do
      (10 + waxOn) `shouldBe` 1135
    it "(+10) waxOn == 1135" $ do
      ((+10) waxOn) `shouldBe` 1135
    it "(-) 15 waxOn == -1110" $ do
      ((-) 15 waxOn) `shouldBe` -1110
    it "(-) waxOn 15 == 1110" $ do
      ((-) waxOn 15) `shouldBe` 1110
    it "triple waxOn == 3375" $ do
      triple waxOn `shouldBe` 3375
    it "triple waxOnWhere == 3375" $ do
      triple waxOnWhere `shouldBe` 3375
    it "waxOff 10 == 30" $ do
      waxOff 10 `shouldBe` 30
    it "waxOff (-50) == 30" $ do
      waxOff (-50) `shouldBe` -150
    
    