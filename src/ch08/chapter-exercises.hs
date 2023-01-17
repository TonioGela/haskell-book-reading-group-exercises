module Ch08.ChapterExercises where

-- Review of types
a = [[True, False], [True, True], [False, True]]
a :: [[Bool]]

b = [[3 == 3], [6 > 5], [3 < 4]]
b :: [[Bool]]

func :: [a] -> [a] -> [a]
func x y = x ++ y
-- x and y must be of the same type
-- x and y must both be lists
-- if x is a String then y must be a String

c = func "Hello" "World"

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- "woops mrow woohoo!"
d = appedCatty "woohoo!"

-- "1 mrow haha"
e = frappe "1"

-- "woops mrow 2 mrow haha"
f = frappe (appedCatty "2")

-- "woops mrow blue mrow haha"
g = appedCatty (frappe "blue")

-- "pink mrow haha mrow green mrow woops mrow blue"
h = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))

-- "are mrow Pugs mrow awesome"
i = cattyConny (flippy "Pugs" "are") "awesome"

-- Recursion
dividedBy :: Integral a => a -> a -> a
dividedBy num den = go num den 0
  where
    go num' den' acc
      | den' > num' = acc
      | otherwise = go (num' - den') den' (acc + 1)

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo n = go n 0
  where
    go 0 acc = acc
    go n' acc = go (n' - 1) (acc + n')

multiplyWithSum :: (Integral a) => a -> a -> a
multiplyWithSum a b = go a b 0
  where
    go _ 0 acc = acc
    go a' b' acc = go a' (b' - 1) (acc + a')

data DividedResult = Result Integer | DividedByZero deriving Show
dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num den = go num den 0
  where
    go _ 0 _ = DividedByZero
    go num' den' acc
      | den' > num' = Result acc
      | otherwise = go (num' - den') den' (acc + 1)

mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))
