module Ch03.ChapterExercises where

-- Reading syntax

-- ok
fn1 = concat [[1, 2, 3], [4, 5, 6]]

-- wrong, missing parenthesis
-- fn2 = ++ [1, 2, 3] [4, 5, 6]
fn2ok = (++) [1, 2, 3] [4, 5, 6]

-- ok
fn3 = (++) "hello" " world"

-- wrong, missing double quotes
-- fn4 = ["hello" ++ " world]
fn4 = ["hello" ++ " world"]

-- wrong, arguments are swapped
-- fn5 = 4 !! "hello"
fn5 = "hello" !! 4

-- ok
fn6 = (!!) "hello" 4

-- wrong, should be the first argument?
-- fn7 = take "4 lovely"
fn7 = take 4 "lovely"

-- ok
fn8 = take 3 "awesome"

set1 = concat [[1 * 6], [2 * 6], [3 * 6]]
res1 = [6, 12, 18]

set2 = "rain" ++ drop 2 "elbow"
res2 = "rainbow"

set3 = 10 * head [1, 2, 3]
res3 = 10

set4 = (take 3 "Julie") ++ (tail "yes")
res4 = "Jules"

set5 = concat [tail [1, 2, 3],
                tail [4, 5, 6],
                tail [7, 8, 9]]
res5 = [2, 3, 5, 6, 8, 9]

-- Building functions
res9 = "Curry is awesome!"
fn9 = "Curry is awesome" ++ "!"

res10 = "y"
fn10 = head $ drop 4 "Curry is awesome!"

res12 = "awesome!"
fn12 = drop 9 "Curry is awesome!"

res13 = "Curry is awesome!"
fn13 s = s ++ "!"

res14 = "y"
fn14 s = head $ drop 4 s

res15 = "awesome!"
fn15 s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 3

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i

rvrs = awesome ++ space ++ is ++ space ++ curry
  where i = "Curry is awesome"
        curry = take 5 i
        is = drop 6 $ take 8 i
        awesome = drop 9 i
        space = drop 5 $ take 6 i
