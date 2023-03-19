module Chapter14 () where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function ()
import Data.List (sort)
import Data.Char (isAlpha, toUpper)
import Chapter13 (handleGuess, fillInCharacter, Puzzle (..))



dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


divideCheck :: IO ()
divideCheck = hspec $ do
  describe "Division" $ do
    it "4 is divisible by 2" $ do
      snd (dividedBy 4 2) `shouldBe` 0
    it "5 divided by 2 has remainder 1" $ do
      snd (dividedBy 5 2) `shouldBe` 1
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)


---TODO write this function and write tests for it
---multiply two numbers by recursively sum them
multiply :: Integral a => a -> a -> a
multiply x y
  | x > 0     = y + multiply (x - 1) y
  | x < 0     = negate y + multiply (x + 1) y
  | otherwise = 0

multiplyCheck :: IO ()
multiplyCheck = hspec $ do
  describe "Multiplication" $ do
    it "0 multiplied by anything is zero" $ do
      property $ \x -> multiply 0 x == (0 ::Int )
    it "x multiplied by 1 is x" $
       property $ \x -> multiply 1 x == (x ::Int )
 --   it "multiplication is commutative" $


---Some generators

genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

---Using QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

---------------
---Exercises---
---------------

---Validating numbers into words

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined

----------------------
---Using QuickCheck---
----------------------

---Exercise 1---
prop_doubleHalf :: (Eq a, Fractional a) => a -> Bool
prop_doubleHalf x =  (x / 2) * 2 == x

checkDoubleHalf :: IO ()
checkDoubleHalf = quickCheck (prop_doubleHalf :: Float -> Bool)

---Exercise 2---
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_sort :: Ord a => [a] -> Bool
prop_sort = listOrdered . sort

checkSort :: IO ()
checkSort = do
  quickCheck (prop_sort :: [Integer] -> Bool)
  quickCheck (prop_sort :: [String] -> Bool)

---Exercises 3 and 4---
binOpAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
binOpAssociative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

binOpCommutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
binOpCommutative op x y = x `op` y == y `op` x

checkPlusAssociative :: IO ()
checkPlusAssociative = quickCheck (binOpAssociative (+) :: Integer
                                                           -> Integer
                                                           -> Integer
                                                           -> Bool)

checkPlusCommutative :: IO ()
checkPlusCommutative = quickCheck (binOpCommutative (+) :: Integer
                                                           -> Integer
                                                           -> Bool)

checkMultAssociative :: IO ()
checkMultAssociative = quickCheck (binOpAssociative (*) :: Integer
                                                           -> Integer
                                                           -> Integer
                                                           -> Bool)

checkMultCommutative :: IO ()
checkMultCommutative = quickCheck (binOpCommutative (*) :: Integer
                                                           -> Integer
                                                           -> Bool)

---Exercise 5---

genPosInt :: Gen Integer
genPosInt =  (arbitrary :: Gen Integer) `suchThat` ( /= 0)

prop_quotRem :: (Eq a, Integral a) => a -> a -> Bool
prop_quotRem x y = quot x y * y + rem x y == x

prop_divMod :: (Eq a, Integral a) => a -> a -> Bool
prop_divMod x y = div x y * y + mod x y == x

checkQuotRem :: IO ()
checkQuotRem = quickCheck $ forAll genPosInt (forAll genPosInt . prop_quotRem)

checkDivMod :: IO ()
checkDivMod = quickCheck $ forAll genPosInt (forAll genPosInt . prop_divMod)

---Exercise 6---

powAssociative :: (Num a, Eq a, Integral b, Integral c) => a -> b -> c -> Bool
powAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

---(0.0, 0, 0) fails
checkPowAssociative :: IO ()
checkPowAssociative = quickCheck (powAssociative :: Double
                                                    -> Integer
                                                    -> Integer
                                                    -> Bool)

powCommutative :: (Num a, Eq a, Integral a) => a -> a -> Bool
powCommutative x y = x ^ y == y ^ x

---(0, 1) fails
checkPowCommutative :: IO ()
checkPowCommutative = quickCheck (powCommutative :: Integer
                                                    -> Integer
                                                    -> Bool)

---Exercise 8---
propEvalCompose :: Eq c => (Fun b c) -> (Fun a b) -> a -> Bool
propEvalCompose (Fun _ f) (Fun _ g) a = (f $ g $ a) == (f . g $ a)

checkEvalCompose :: IO ()
checkEvalCompose = quickCheck (propEvalCompose :: (Fun Integer Integer)
                                                  -> (Fun Integer Integer)
                                                  -> Integer
                                                  -> Bool)

---Exercise 9---
propFoldCons :: Eq a => [a] -> [a] -> Bool
propFoldCons xs ys = foldr (:) xs ys == (++) xs ys

checkFoldCons :: IO ()
checkFoldCons = quickCheck (propFoldCons :: [String] -> [String] -> Bool)

propConcat :: Eq a => [[a]] -> Bool
propConcat xs = foldr (++) [] xs == concat xs

checkConcat :: IO ()
checkConcat = quickCheck (propConcat :: [[String]] -> Bool)

---Exercise 10---

propLength :: Int -> [a] -> Bool
propLength n xs = length (take n xs) == n

checkLenght :: IO ()
checkLenght = quickCheck (propLength :: Int -> [String] -> Bool)

---Exercise 11---

propShowRead :: (Read a, Show a, Eq a) => a -> Bool
propShowRead x = (read . show $ x) == x

checkShowRead :: IO ()
checkShowRead = quickCheck (propShowRead :: Integer -> Bool)

-----------------
---Idempotence---
-----------------

---Exercises 1 and 2---
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs)
  | isAlpha x = toUpper x : xs
  | otherwise = x : capitalizeWord xs


twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

propCapitalizeWord :: String -> Bool
propCapitalizeWord st = (capitalizeWord st == twice capitalizeWord st)
                        && (capitalizeWord st == fourTimes capitalizeWord st)

propSortIdempotence :: Ord a => [a] -> Bool
propSortIdempotence xs = (sort xs == twice sort xs)
                        && (sort xs == fourTimes sort xs)

checkIdempotence :: IO ()
checkIdempotence = do
  quickCheck propCapitalizeWord
  quickCheck (propSortIdempotence :: [String] -> Bool)

--------------------------------------------------
---Make a Gen random generator for the datatype---
--------------------------------------------------

data Fool = Fulse | Frue
  deriving (Eq, Show)

genFoolBalanced :: Gen Fool
genFoolBalanced = elements [Fulse, Frue]

genFoolUnbalanced :: Gen Fool
genFoolUnbalanced = frequency [ (1, pure Fulse)
                              , (2, pure Frue)]
---------------------
---Hangman testing---
---------------------

fillInCharacterTest :: IO ()
fillInCharacterTest = hspec $
    describe "fillInCharacter unit tests" $ do

      it "handles correct guesses" $ do
        let word = "cac"
            guess     = 'a'
            puzzle    = Puzzle word [Nothing,  Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Nothing, Just 'a', Nothing] [guess]
        fillInCharacter puzzle guess `shouldBe` expectedPuzzle

      it "handles incorrect guesses" $ do
        let word = "cac"
            guess     = 't'
            puzzle    = Puzzle word [Nothing,  Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Nothing, Nothing, Nothing] [guess]
        fillInCharacter puzzle guess `shouldBe` expectedPuzzle

      it "handles repeated guesses" $ do
        let word = "cac"
            guess     = 'a'
            puzzle    = Puzzle word [Nothing, Just 'a', Nothing] [guess]
            expectedPuzzle =
              Puzzle word [Nothing, Just 'a', Nothing] [guess, guess]
        fillInCharacter puzzle guess `shouldBe` expectedPuzzle

      it "handles multiple correct chars" $ do
        let word = "cac"
            guess     = 'c'
            puzzle    = Puzzle word [Nothing,  Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Just 'c', Nothing, Just 'c'] [guess]
        fillInCharacter puzzle guess `shouldBe` expectedPuzzle


handleGuessTest :: IO ()
handleGuessTest = hspec $
    describe "handleGuess unit tests" $ do

      it "updates a puzzle when a correct guess is made" $ do
        let word = "cac"
            guess     = 'a'
            puzzle    = Puzzle word [Nothing,  Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Nothing, Just 'a', Nothing] [guess]
        do
          updatePuzzle <- handleGuess puzzle guess
          updatePuzzle `shouldBe` expectedPuzzle

      it "updates a puzzle when an incorrect guess is made" $ do
        let word = "cac"
            guess     = 't'
            puzzle    = Puzzle word [Nothing,  Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Nothing, Nothing, Nothing] [guess]
        do
          updatePuzzle <- handleGuess puzzle guess
          updatePuzzle `shouldBe` expectedPuzzle

      it "does not update a puzzle when a (correct) guess was already made" $ do
        let word = "cac"
            guess     = 'a'
            puzzle    = Puzzle word [Nothing,  Just 'a', Nothing] [guess]
        do
          updatePuzzle <- handleGuess puzzle guess
          updatePuzzle `shouldBe` puzzle

---L'implementazione data da loro fa fallire questo test che, a mio parere, non dovrebbe fallire
{-
      it "does not update a puzzle when a guess was already made" $ do
        let word = "cac"
            guess     = 't'
            puzzle    = Puzzle word [Nothing,  Just 'a', Nothing] ['t']
        do
          updatePuzzle <-  handleGuess puzzle guess
          updatePuzzle `shouldBe` puzzle
-}
