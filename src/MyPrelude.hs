{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module MyPrelude where

import Prelude hiding (
  (++),
  (!!),
  ($),
  (^),
  head,
  last,
  tail,
  init,
  null,
  const,
  rem,
  odd,
  even,
  length,
  concat,
  concatMap,
  reverse,
  flip,
  map,
  filter,
  take,
  drop,
  takeWhile,
  dropWhile,
  splitAt,
  elem,
  compare,
  maximum,
  minimum,
  zip,
  zipWith,
  foldr,
  foldl,
  sum,
  scanr,
  scanl)

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (a:as) b = a : (as ++ b)

(!!) :: [a] -> Int -> a
(!!) [] _ = undefined
(!!) (a:_) 0 = a
(!!) (_:as) b = as !! (b - 1)

($) :: (a -> b) -> a -> b
($) f = f

(^) :: (Integral a) => a -> a -> a
(^) a 0
  | a > 0     = 1
  | otherwise = -1
(^) a b
  | a == 1    = 1
  | a == -1   = -1
  | b == 0    = a
  | otherwise = a * (a ^ (b - 1))

head :: [a] -> a
head [] = undefined
head (a:_) = a

last :: [a] -> a
last [] = undefined
last [a] = a
last (_:as) = last as

tail :: [a] -> [a]
tail [] = undefined
tail (_:as) = as

init :: [a] -> [a]
init [] = undefined
init [_] = []
init (a:as) = a:init as

null :: [a] -> Bool
null [] = True
null _ = False

const :: a -> b -> a
const a _ = a

rem :: Integral a => a -> a -> a
rem _ 0 = undefined
rem a b
  | a >= b = rem (a - b) b
  | otherwise = a

odd :: Integral a => a -> Bool
odd a = (==) 1 $ rem a 2

even :: Integral a => a -> Bool
even a = (==) 0 $ rem a 2

length :: [a] -> Int
length [] = 0
length a = go a 0
  where
    go [] _ = 0
    go [_] len = len + 1
    go (_:as) len = go as (len + 1)

concat :: [[a]] -> [a]
concat [] = []
concat (a:as) = a ++ concat as

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (a:as) = f a ++ concatMap f as

reverse :: [a] -> [a]
reverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a:map f as

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (a:as)
  | f a = a:filter f as
  | otherwise = filter f as

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (a:as) = a:take (n - 1) as

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 a = a
drop n (_:as) = drop (n - 1) as

splitAt :: Int -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt 0 as = ([], as)
splitAt n (a:as) = (a:fst nextSplit, snd nextSplit)
  where nextSplit = splitAt (n - 1) as

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (a:as)
  | f a = a:takeWhile f as
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (a:as)
  | f a = dropWhile f as
  | otherwise = a:as

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem v (a:as)
  | v == a = True
  | otherwise = elem v as

compare :: (Ord a) => a -> a -> Ordering
compare a b
  | a > b     = GT
  | a < b     = LT
  | otherwise = EQ

maximum :: Ord a => [a] -> a
maximum [] = undefined
maximum [a] = a
maximum [a, b]
  | compared == GT = a
  | compared == LT = b
  | otherwise = a
  where compared = compare a b
maximum (a:b:as)
  | compared == GT = maximum (a:as)
  | compared == LT = maximum (b:as)
  | otherwise = maximum (a:as)
  where compared = compare a b

minimum :: Ord a => [a] -> a
minimum [] = undefined
minimum [a] = a
minimum [a, b]
  | compared == GT = b
  | compared == LT = a
  | otherwise = a
  where compared = compare a b
minimum (a:b:as)
  | compared == GT = minimum (b:as)
  | compared == LT = minimum (a:as)
  | otherwise = minimum (a:as)
  where compared = compare a b

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a, b):zip as bs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a:as) (b:bs) = f a b:zipWith f as bs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ i [] = i
foldr f i (a:as) = f a (foldr f i as)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ i [] = i
foldl f i (a:as) = f (foldl f i as) a

sum :: Num a => [a] -> a
sum = foldr (+) 0
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (a:as) = a + sum as

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ i [] = [i]
scanr f i (a:as) = f a (head nextRes):nextRes
  where nextRes = scanr f i as

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ i [] = [i]
scanl f i (a:as) = f (head nextRes) a:nextRes
  where nextRes = scanl f i as
