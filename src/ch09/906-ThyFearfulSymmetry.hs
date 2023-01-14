module Ch09.ThyFearfulSymmetry where

import Data.Char

-- myWords "sheryl wants fun"
myWords :: String -> [String]
myWords (a:as)
  | (isSpace a) = myWords as
  | otherwise = takeWhile (not . isSpace) [a:as]
