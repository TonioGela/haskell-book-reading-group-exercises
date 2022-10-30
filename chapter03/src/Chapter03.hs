module Chapter03(fun1A, fun1B, fun1C, thirdLetter, letterIndex, rvrs) where

fun1A :: String -> String
fun1A s = s ++ "!"

fun1B :: String -> String
fun1B s = take 1 $ drop 4 s

fun1C :: String -> String
fun1C = drop 9 

thirdLetter :: String -> Char
thirdLetter s = (!!) s 3 

letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome!"

rvrs :: String -> String
rvrs s = awesome ++ " " ++ is ++ " " ++ _curry
    where
        slice i j = take (j - i) $ drop i s
        _curry = slice 0 5
        is = slice 6 8
        awesome = slice 9 16