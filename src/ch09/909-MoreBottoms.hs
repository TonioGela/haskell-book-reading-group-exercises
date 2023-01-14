module Ch09.MoreBottoms where

-- returns bottom
a1 = take 1 $ map (+1) [undefined, 2, 3]

-- returns a value, [2]
a2 = take 1 $ map (+1) [1, undefined, 3]

-- returns bottom
a3 = take 2 $ map (+1) [1, undefined, 3]

-- this function says, for each char, if that char is a vocal
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- returns a list of squares of input numbers
a4 = map (^2) [1..10]

-- get the minimum value from each sub list,, returning a list of number
a5 = map minimum [[1..10], [10..20], [20..30]]

-- sum each sub list, returning a list of sums
a6 = map sum [[1..5], [1..5], [1..5]]