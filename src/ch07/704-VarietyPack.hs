module Ch07.VarietyPack where

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- What is the type of k?
k :: (a, b) -> a

-- What is the type of k2? Is it the same type as k1 or k3? Types are different
k2 :: [Char]
k3 :: Num a => a

-- Of k1, k2, k3, which will return the number 3 as the result? k3


-- Fill in the definition
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
