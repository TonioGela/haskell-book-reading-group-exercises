module Chapter12 () where

import Data.Char ( toLower )
import Data.List ((\\))
import Data.Bifunctor

-----------------------
---String Processing---
-----------------------

---Exercise 1
notThe :: String -> Maybe String
notThe st = if map toLower st == "the" then Nothing else Just st

nothingToa :: Maybe String -> String
nothingToa (Just a) = a
nothingToa Nothing = "a"

replaceThe :: String -> String
replaceThe st = unwords $ map (nothingToa . notThe) $ words st


---Exercise 2---
vowels :: String
vowels = "aeiouy"

consonants :: [Char]
consonants = ['a'..'z'] \\ vowels

isVowel :: Char -> Bool
isVowel = (`elem` vowels) . toLower

isConsonant :: Char -> Bool
isConsonant =  (`elem` consonants) . toLower

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go ("the":x:xs) = if isVowel . head $ x then 1 + go xs else go (x:xs)
        go (_:xs) = go xs
        go _ = 0

---Exercise 3---
countVowel :: String -> Int
countVowel = length . filter isVowel

-----------------------
---Validate the word---
-----------------------

countConsonant :: String -> Int
countConsonant = length . filter isConsonant


newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord st = if (countConsonant st - countVowel st) < 0
              then Nothing else Just . Word' $ st

-----------------------
---It’s only Natural---
-----------------------

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = integerToNat (n - 1) >>= Just . Succ

-----------------------------
---Small library for Maybe---
-----------------------------

---Exercise 1---
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

---Exercise 2---
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

---Exercise 3---
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

---Exercise 4---
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

---Exercise 5---
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

---Exercise 6---
maybeCons :: Maybe a ->  Maybe [a] -> Maybe [a]
maybeCons (Just a) (Just xs) = Just (a:xs)
maybeCons _ _ = Nothing

---The same function but using the fact that Maybe a is an applicative
maybeCons' :: Maybe a ->  Maybe [a] -> Maybe [a]
maybeCons' ma mxs = (:) <$> ma <*> mxs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr maybeCons (Just [])

------------------------------
---Small library for Either---
------------------------------

---Exercise 1---
lefts' :: [Either a b] -> [a]
lefts' = fst . partitionEithers'

---Exercise 2---
rights' :: [Either a b] -> [b]
rights' = snd . partitionEithers'

---Exercise 3---
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (flip f) ([],[])
  where f xs = either' (\a -> first (a: ) xs)
                       (\b -> second (b: ) xs)


---Exercise 4---
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

---Exercise 5---
either' ::  (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

---Exercise 6---
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-------------
---Unfolds---
-------------

---Exercise 1---
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

---Exercise 2---
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  (Just (a1,b1)) -> a1 : myUnfoldr f b1

---Exercise 3---
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (f a, a))

------------------------------------------
---Finally something other than a list!---
------------------------------------------


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

---Exercise 1---
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing -> Leaf
               (Just (a1, b1, a2)) -> Node (unfold f a1) b1 (unfold f a2)

---Exercise 2---
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\m -> if m >= n
                       then Nothing else Just (m + 1, m , m + 1)) 0

