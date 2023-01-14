module Ch09.ThyFearfulSymmetry where

import Data.Char

-- 1.
myWords :: String -> [String]
myWords [] = []
myWords (' ':as) = myWords as
myWords as = takeWhile (not.isSpace) as : myWords (dropWhile (not.isSpace) as)

-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines ('\n':as) = myLines as
myLines as = takeWhile (/= '\n') as : myLines (dropWhile (/= '\n') as)

shouldEqual = [
  "Tyger Tyger, burning bright",
  "In the forests of the night",
  "What immortal hand or eye",
  "Could frame thy fearful symmetry?"
  ]
main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
  == shouldEqual)

-- 3.
explode :: Char -> String -> [String]
explode _ [] = []
explode c (a:as)
  | c == a = explode c as
  | otherwise = takeWhile (/= c) (a:as) : myLines (dropWhile (/= c) (a:as))
