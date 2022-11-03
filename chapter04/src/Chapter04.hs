module Chapter04 (myLength, awesome, also, allAwesome, isPalindrome, myAbs, f, f') where

myLength :: [a] -> Int
myLength = length

awesome :: [String]
awesome = ["Paphuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

myAbs :: Integer -> Integer
myAbs n = if n < 0 then -n else n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((b, d), (a, c)) where
    a = fst t1
    b = snd t1
    c = fst t2
    d = snd t2

f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (a, b) (c, d) = ((b, d), (a, c))
