{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module MyPrelude where

import Prelude hiding (
  (++),
  (!!),
  head,
  last,
  tail,
  init,
  null,
  length,
  concat,
  concatMap,
  reverse,
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
  foldl)

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (a:as) b = a : (as ++ b)

(!!) :: [a] -> Int -> a
(!!) (a:_) 0 = a
(!!) (_:as) b = as !! (b - 1)

head :: [a] -> a
head (a:_) = a

last :: [a] -> a
last [a] = a
last (_:as) = last as

tail :: [a] -> [a]
tail (_:as) = as

init :: [a] -> [a]
init [_] = []
init (a:as) = a:init as

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Int
length [] = 0
length a = go a 0
  where
    go [_] len = len + 1
    go (_:as) len = go as (len + 1)

concat :: [[a]] -> [a]
concat [] = []
concat (a:as) = a ++ concat as

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (a:as) = (f a) ++ (concatMap f as)

reverse :: [a] -> [a]
reverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a:map f as

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (a:as)
  | f a = a:filter f as
  | otherwise = filter f as

take :: Int -> [a] -> [a]
take 0 _ = []
take n (a:as) = a:take (n - 1) as

drop :: Int -> [a] -> [a]
drop 0 a = a
drop n (_:as) = drop (n - 1) as

splitAt :: Int -> [a] -> ([a], [a])
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
maximum (a:b:[])
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
minimum (a:b:[])
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
zip (a:as) (b:bs) = (a, b):(zip as bs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a:as) (b:bs) = f a b:zipWith f as bs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i [] = i
foldr f i (a:as) = f a (foldr f i as)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i [] = i
foldl f i (a:as) = f (foldl f i as) a
