module Chapter04.Exercises where

awesome = ["Papuchon", "curry", ":)"]

also = ["Quake", "The Simons"]

allAwesome = [awesome, also]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x