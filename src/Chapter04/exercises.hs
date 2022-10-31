module Chapter04.Exercises where

awesome = ["Papuchon", "curry", ":)"]

also = ["Quake", "The Simons"]

allAwesome = [awesome, also]

-- 8.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ab cd = (,) (snd ab, snd cd) (fst ab, fst cd)
