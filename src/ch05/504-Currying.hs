module Ch05.Currying where

f :: a -> a -> a -> a
x :: Char
-- :t f x?
f' :: Char -> Char -> Char

g :: a -> b -> c -> b
-- :t g 0 'c' "woot"?
g' :: Char

h :: (Num a, Num b) => a -> b -> b
-- :t h 1.0 2?
h' :: Num a => a 

h2 :: (Num a, Num b) => a -> b -> b
-- :t h2 1 (5.5 :: Double)?
h2' :: Double

jackal :: (Ord a, Eq b) => a -> b -> a
-- :t jackal "keyboard" "has the word jackal in it"?
jackal' :: String

jackal2 :: (Ord a, Eq b) => a -> b -> a
-- :t jackal2 "keyboard"?
jackal2' :: Eq b => b -> String

kessel :: (Ord a, Num b) => a -> b -> a
-- :t kessel 1 2?
kessel' :: Num a => a

kessel2 :: (Ord a, Num b) => a -> b -> a
-- :t kessel2 1 (2 :: Integer)?
kessel2' :: Num a => a

kessel3 :: (Ord a, Num b) => a -> b -> a
-- :t kessel3 (1 :: Integer) 2?
kessel3' :: Integer

-- end of file
f = undefined
x = undefined
f' = undefined
g = undefined
g' = undefined
h = undefined
h' = undefined
h2 = undefined
h2' = undefined
jackal = undefined
jackal' = undefined
jackal2 = undefined
jackal2' = undefined
kessel = undefined
kessel' = undefined
kessel2 = undefined
kessel2' = undefined
kessel3 = undefined
kessel3' = undefined
