module Ch10.UnderstandingFolds where

-- foldr (*) 1 [1..5]
-- will return the same result as:
-- foldl (flip (*)) 1 [1..5]
-- foldl (*) 1 [1..5]

-- foldl (flip (*)) 1 [1..3]
-- evaluation steps:
-- flip (*) (flip (*) (flip (*) 1 1) 2) 3

-- One difference between foldr and foldl is:
-- foldr, but not foldl, associates to the right

-- Folds are catamorphisms, which means they are generally used to
-- reduce structure

-- The following are simple folds very similar to what you’ve already seen, but each has at least one error
-- foldr (++) ["woot", "WOOT", "woot"]
f1 = foldr (++) "" ["woot", "WOOT", "woot"]

-- foldr max [] "fear is the little death"
f2 = foldr max ' ' "fear is the little death"

-- foldr and True [False, True]
f3 = foldr (\x y -> and [x, y]) True [False, True]

-- foldr (||) True [False, True]
f4 = foldr (||) True [False, True]

-- foldl ((++) . show) "" [1..5]
f5 = foldl (\x y -> x ++ (show y)) "" [1..5]

-- foldr const 'a' [1..5]
f6 = foldr const 0 [1..5]

-- foldr const 0 "tacos"
f7 = foldr const ' ' "tacos"

-- foldl (flip const) 0 "burritos"
f8 = foldl (flip const) ' ' "burritos"

-- foldl (flip const) 'z' [1..5]
f9 = foldl (flip const) 0 [1..5]
