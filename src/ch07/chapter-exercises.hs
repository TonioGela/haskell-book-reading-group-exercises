module Ch07.ChapterExercises where

-- 1. A polymorphic function may resolve to values of different types, depending on inputs
-- 2. Two functions named f and g have types Char -> String and String -> [String] respectively.
--    The composed function g . f has the type Char -> [String]
-- 3. A function f has the type Ord a => a -> a -> Bool and we apply it to one numeric value.
--    f 1 :: Num a, Ord a => a -> Bool 
-- 4. A function with the type (a -> b) -> c is a higher-order function

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit :: Integral a => a -> a
tensDigit x = snd . divMod x $ 10