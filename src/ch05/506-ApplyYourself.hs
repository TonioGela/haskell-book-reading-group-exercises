module Ch05.ApplyYourself where

myConcat :: String -> String
myConcat x = x ++ " yo"

myMult :: Fractional a => a -> a
myMult x = (x/3) * 5

myTake :: Int -> String
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > (length [1..10])

myAlph :: Char -> Bool
myAlph x = x < 'z'
