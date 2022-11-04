module Specs.Ch03Spec (spec) where


import Test.Hspec
import Ch03 (f1, f1a, f1b, f1c, thirdLetter, letterIndex, rvrs);

spec :: Spec
spec = do
  describe "3.8 Building functions" $ do
    describe "1" $ do
      it "#" $ do 
        f1 "Hello World" `shouldBe` "ello World"
    describe "1.a" $ do
      it "#" $ do
        f1a "Curry is awesome" `shouldBe` "Curry is awesome!"
    describe "1.b" $ do
      it "#" $ do
        f1b "Curry is awesome" `shouldBe` 'y'
    describe "1.c" $ do
      it "#" $ do
        f1c "Curry is awesome" `shouldBe` "awesome!"

    describe "3 - thirdLetter" $ do
      it "returns 3rd letter of 'Curry is awesome'" $ do
        thirdLetter "Curry is awesome" `shouldBe` 'r'
      it "returns 3rd letter of 'Pluto is awesome'" $ do
        thirdLetter "Pluto is awesome" `shouldBe` 'u'

    describe "4 - letterIndex" $ do
      it "returns 1st letter" $ do
        letterIndex 0 `shouldBe` 'C'
      it "returns 9th letter" $ do
        letterIndex 10 `shouldBe` 'w'

    describe "5 - rvrs" $ do
      it "returns 1st letter" $ do
        rvrs "Curry is awesome" `shouldBe` "awesome is Curry"