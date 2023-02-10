module ChapterThree where

-- Ex. 3
thirdLetter :: [Char] -> Char
thirdLetter s = s !! 2

-- Ex. 4
letterIndex :: Int -> Char
letterIndex n = let s = "Curry is awesome" in s !! (n - 1)

-- Ex. 5
rvrs :: [Char] -> [Char]
rvrs s =
    let
        a = take 5 s
        b = drop 5 (take 9 s)
        c = drop 9 s
     in
        c ++ b ++ a
