module Ch09.Filtering where

f1 = filter (\x -> (rem x 3) == 0)

f2 = length . f1

f3 = filter (not . (\x -> (x == "the" || x == "a" || x == "an"))) . words