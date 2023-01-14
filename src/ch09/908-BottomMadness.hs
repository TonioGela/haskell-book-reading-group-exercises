module Ch09.BottomMadness where

-- 1. returns bottom
a1 = [x^y | x <- [1..5], y <- [2, undefined]]

-- 2. returns a value, [1]
a2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- 3. returns bottom
a3 = sum [1, undefined, 3]

-- 4. returns a value, 3
a4 = length [1, 2, undefined]

-- 5. returns bottom
a5 = length $ [1, 2, 3] ++ undefined

-- 6. returns a value [2]
a6 = take 1 $ filter even [1, 2, 3, undefined]

-- 7. returns bottom
a7 = take 1 $ filter even [1, 3, undefined]

-- 8. returns a value, [1]
a8 = take 1 $ filter odd [1, 3, undefined]

-- 9. returns a value [1, 3]
a9 = take 2 $ filter odd [1, 3, undefined]

-- 10. returns bottom
a10 = take 3 $ filter odd [1, 3, undefined]


-- Is it in normal form?

-- NF
a11 = [1, 2, 3, 4, 5]

-- WHNF
a12 = 1 : 2 : 3 : 4 : _

-- neither
a13 = enumFromTo 1 10

-- neither
a14 = length [1, 2, 3, 4, 5]

-- WHNF
a15 = sum (enumFromTo 1 10)

-- WHNF
a16 = ['a'..'m'] ++ ['n'..'z']

-- WHNF
a17 = (_, 'b')
