module Ch09.ChapterExercises where

import Data.Char

myIsUpper :: Char -> Bool
myIsUpper c = elem c ['A'..'Z']
myToUpper :: Char -> Char
myToUpper c
  | elem c ['a'..'z'] = chr $ subtract 32 $ ord c
  | otherwise = c

f2 = filter myIsUpper

f3 :: String -> String
f3 [] = []
f3 (s:ss) = myToUpper s:ss

f4 :: String -> String
f4 [] = []
f4 (s:ss) = myToUpper s:f4 ss

f5 :: String -> Char
f5 [] = undefined
f5 (s:ss) = myToUpper s

f6 :: String -> Char
f6 = myToUpper . head

-- Ciphers
caesar :: Int -> String -> String
caesar shift = map (\x -> chr $ (ord x - base + shift) `mod` 26 + base)
  where base = ord 'a'

unCaesar :: Int -> String -> String
unCaesar shift = map (\x -> chr $ (ord x - base - shift) `mod` 26 + base)
  where base = ord 'a'

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (a:as) = a && myOr as

myOr :: [Bool] -> Bool
myOr [] = False
myOr (a:as) = a || myOr as

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = True
myAny f (a:as) = f a || myAny f as

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (a:as)
  | e == a = True
  | otherwise = myElem e as

myElem' :: Eq a => a -> [a] -> Bool
myElem' e a = myAny (\x -> x == e) a

myReverse :: [a] -> [a]
myReverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

-- squish is the same as concat function
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (a:as) = a ++ myConcat as

-- squishMap is the same as concatMap function
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap _ [] = []
myConcatMap f (a:as) = (f a) ++ (myConcatMap f as)

-- squishAgain is the same as concat function
concatAgain = myConcat


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- THIS IS WIP