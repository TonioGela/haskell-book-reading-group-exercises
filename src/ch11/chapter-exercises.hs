module Ch11.ChapterExercises where

import Data.Char

-- Weekday is a type with five data constructors
data Weekday =
         Monday
       | Tuesday
       | Wednesday
       | Thursday
       | Friday

f :: Weekday -> String
f Friday = "Miller Time"

-- Types defined with the data keyword must begin with a capital letter

-- The function g is recursive and may not terminate
g xs = xs !! (length xs - 1)

caesarCipher :: Int -> String -> String
caesarCipher i = map (chr.(+97).(flip mod 26).(subtract 97).(+i).ord)

-- TODO
-- vigenereCipher :: String -> String -> String
-- vigenereCipher (a:as) (b:bs) = 

cipher :: String -> String -> String
cipher = zipWith (\l a -> caesarCipher (ord l - ord 'A') a) (repeat key)
cipher ks = snd . mapAccumL f (repeat ks) 
    where 
        f ks ' ' = (ks, ' ')
        f (k:ks) c = (ks, caesarCipher (ord k - ord 'A) c)


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ass@(a:as) (b:bs)
  | a == b    = isSubseqOf as bs
  | otherwise = isSubseqOf ass bs

capitalizeWord :: String -> String
capitalizeWord (a:as) = toUpper a:as

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capitalizeWord x)) . words

-- TODO ...