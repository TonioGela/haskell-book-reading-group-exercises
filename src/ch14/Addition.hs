module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) == 4 `shouldBe` True
  
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do
    it "10 multiplied by 0 is 0" $ do 
      mul 10 0 `shouldBe` 0
    it "10 multiplied by 1 is 10" $ do 
      mul 10 1 `shouldBe` 10
    it "41 multiplied by 312 is 12792" $ do 
      mul 41 312 `shouldBe` 12792

  describe "Property" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
    

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

mul :: (Eq a, Num a) => a -> a -> a
mul 0 _ = 0
mul _ 0 = 0
mul 1 b = b
mul a 1 = a
mul a b = (+) a $ mul a $ b - 1

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

oneOrTwo :: Gen Int
oneOrTwo = choose (1,2)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  l <- arbitrary
  r <- arbitrary
  elements [Left l, Right r]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
