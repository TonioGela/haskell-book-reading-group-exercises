
module Core.Spec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Examples" $ do
    it "true == true" $ do
      True `shouldBe` True