module Ch06.ChapterExercises where

import Data.List

-- Multiple choice
-- The Eq class makes equality tests possible
-- The typeclass Ord is a subclass of Eq
-- Suppose the typeclass Ord has an operator >. What is the type of >? 
(>) :: Ord a => a -> a -> Bool
(>) = undefined
-- In x = divMod 16 12 the type of 𝑥 is a tuple
-- The typeclass Integral includes Int and Integer numbers


-- Does it typecheck?
-- 1. no, Person' Bool must derive Show typeclass
data Person = Person' Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. no, Mood must derive also Eq typeclass
data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot
                then Blah
                else x
-- 3a. What values are acceptable inputs to that function?
--    type of x is inferred by the (==) function. The second term of the (==) function is Woot
--    but Haskell infer Mood because Mood is the most polymorfic type. Therefore, acceptable inputs 
--    are values of type Mood

-- 3b. What will happen if you try to run settleDown 9? Why?
--    It does not compile because 9 is not a Mood value

-- 3c. What will happen if you try to run Blah > Woot? Why?
--    It does not compile because Blah and Woot don't have an instance of type Ord

-- 4. yes, it compiles
type Subject = String
type Verb = String
type Object = String
data Sentence = SentenceConstructor Subject Verb Object deriving (Eq, Show)
s1 = SentenceConstructor "dogs" "drool"
s2 = SentenceConstructor "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1. it does not compile because "chases" is not a value of type Rocks (the same for True and Yeah)
-- phew = Papu "chases" True
-- 2. it compiles
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. it compiles
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. it does not compile because (>) function needs a value of a type with an instance of Ord  
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'


-- Match the types
-- 1. IDK why it does not compile :-(
-- i :: a
-- i = 1

-- 2. it does not compile because type of 1.0 is Fractional. Num is too much generic
-- f :: Num a => a
-- f = 1.0

-- 3. it compiles
f :: Fractional a => a
f = 1.0

-- 4. it compiles
f2 :: RealFrac a => a
f2 = 1.0

-- 5. it compiles
freud :: Ord a => a -> a
freud x = x

-- 6. it compiles
freud' :: Int -> Int
freud' x = x

-- 7. it does not compile because x could be any type but the function always returns an Int.
-- myX = 1 :: Int
-- sigmund :: a -> a
-- sigmund x = myX

-- 8. it does not compile because x could be a Float type but the function always returns an Int
-- myX = 1 :: Int
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- 9. it compiles
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10. it compiles
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11. it does not compile because mySort accepts only [Char] in the first argument
--     but any type could be passed to mySort due to signifier signature 
-- mySort :: [Char] -> [Char]
-- mySort = sort
-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)


-- Type-Kwon-Do Two: Electric Typealoo
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (==) b $ aToB a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = (aToB a) + (fromInteger i)
