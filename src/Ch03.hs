module Ch03 where


f1 :: String -> String
f1 h = tail h

f1a :: String -> String
f1a h = h ++ "!"

f1b :: String -> Char
f1b h = h !! 4

f1c :: String -> String
f1c h = drop 9 (f1a h)

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = awesome ++ _is_ ++ curry
    where
        awesome = drop 9 x
        _is_ = take 4 $ drop 5 x
        curry = take 5 x