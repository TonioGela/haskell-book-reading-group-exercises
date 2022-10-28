module Chapter03.BuildingFunctions where

appendExclamation x = x ++ "!"

get5th x = x !! 4

drop9 = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2