module Chapter06 () where

import Data.List(sort)

-- ############################################
--  Does is typecheck?
-- ############################################

-- ## 1
data Person = Person Bool 
    deriving Show

printPerson :: Person -> IO ()
printPerson p = putStrLn $ show p


-- ## 2
data Mood = Blah | Whoot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Whoot then Blah else x

-- ## 3
-- a) settleDown accepts Mood values: settleDown :: Mood -> Mood
-- b) settleDown 9 won't typecheck
-- c) Blah > Whoot won't compile due to missing instance for Ord

-- ## 4 (Already compiles)
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drools"
s2 = Sentence "Julie" "loves" "dogs"

-- ############################################
--  Given a datatype declaration what can we do? 
-- ############################################

data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

-- ## 1: missing type constructors
phew = Papu (Rocks "Chases") (Yeah True)

-- ## 2: is ok
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- ## 3: missing Ord instance
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- ############################################
--  Match the types 
-- ############################################

-- ## 1
i :: Num a => a
i = 1

-- "i :: a" does not compile, needs a concrete type

-- ## 2
f :: Float
f = 1.0

-- f' :: Num a => a | could not deduce Fractional
-- f' = 1.0

-- ## 3
f'' :: Fractional a => a
f'' = 1.0

-- ## 4
f''' :: RealFrac a => a
f''' = 1.0

-- ## 5
freud :: a -> a
freud x = x

-- The Ord constraint is not used, could be removed
freud' :: Ord a => a -> a
freud' x = x

-- ## 6
-- The type of the function is now specific to Int
freud'' :: Int -> Int
freud'' x = x

-- ## 7
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- sigmund :: a -> a | does not compile. Int does not conform to a

-- ## 8
-- sigmund :: Num a => a -> a | does not compile. Int is too specific and might not conform to the concrete type of a

-- ## 9
jung :: Ord a => [a] -> a
jung xs = head $ sort xs

jung' :: [Int] -> Int
jung' xs = head $ sort xs

-- ## 10
young :: [Char] -> Char
young xs = head $ sort xs

young' :: Ord a => [a] -> a
young' xs = head $ sort xs

-- ## 11
mysort :: [Char] -> [Char]
mysort = sort

signifier :: [Char] -> Char
signifier xs = head $ sort xs

signifier' :: Ord a => [a] -> a
signifier' xs = head $ sort xs


-- ############################################
--  Type Kwan Do Two
-- ############################################
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fab a b = b == (fab a)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fab n a = (fab a) * (fromInteger n)

