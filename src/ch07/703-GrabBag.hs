module Ch07.GrabBag where

-- 1. Which (two or more) of the following are equivalent? Functions are identical
mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

-- 2. The type of mTh (above) is Num a => a -> a -> a -> a. Which is the type of mTh 3?
mThWith3 :: Num a => a -> a -> a
mThWith3 = mTh1 3

-- 3. Rewrite with anonimous functions
addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

-- Rewrite the following so that it doesn’t use anonymous lambda syntax
mflip f x y = f y x