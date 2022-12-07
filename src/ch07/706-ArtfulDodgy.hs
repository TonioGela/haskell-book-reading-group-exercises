module Ch07.ArtfulDodgy where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- 11
dodgy 1 1

-- 22
dodgy 2 2

-- 21
dodgy 1 2

-- 11
oneIsOne 1

-- 21
oneIsOne 2

-- 21
oneIsTwo 1

-- 22
oneIsTwo 2

-- 31
oneIsOne 3

-- 23
oneIsTwo 3