module Ch04 where
import Data.Char;

awesome = ["Papuchon", "curry", ":)"] 
also = ["Quake", "The Simons"] 
allAwesome = [awesome, also]

length' :: [a] -> Int
length' = length

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

isPalindromeString :: String -> Bool
isPalindromeString x = 
    reverse x' == x'
    where 
        x' = filter (/=' ') x

myAbs :: Integer -> Integer 
myAbs x = if x < 0
        then 
            -1 * x
        else
            x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))