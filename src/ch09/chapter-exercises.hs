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
