module Ch04.ChapterExercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- Given the definition of length above, what would the type signature be?
--  How many arguments, of what type does it take? 1 argument that should be a list
--  What is the type of the result it evaluates to? Integer or Int
-- length :: [a] -> Int

fn1 = length [1, 2, 3, 4, 5]
res1 = 5

fn2 = length [(1, 2), (2, 3), (3, 4)]
res2 = 3

fn3 = length allAwesome
res3 = 2

fn4 = length (concat allAwesome)
res4 = 5

-- the following expression is wrong
-- 6 / length [1, 2, 3]
fixed = 6 `div` length [1, 2, 3]

fn5 = 2 + 3 == 5
fn5 :: Bool

fn6 = length allAwesome == 2
res6 = True

-- length [1, 'a', 3, 'b'] is wrong: elements in the list must be of the same type

fn7 = length allAwesome + length awesome
res7 = 5

fn8 = (8 == 8) && ('b' < 'a')
res8 = False

-- (8 == 8) && 9 is wrong: && operator wants boolean

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0
          then -x
          else x
myAbsWithGuard :: Integer -> Integer
myAbsWithGuard x 
  | x < 0= -x
  | otherwise = x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- Correcting syntax
x9 = (Prelude.+)
f9 xs = w x 1
  where w = length xs
-- \X = x
identity = (\x -> x)

-- f (a b) = A
f10 (a, b) = a

-- Match the function names to their types
show :: Show a => a -> String
show = undefined

(==) :: Eq a => a -> a -> Bool
(==) = undefined

fst :: (a, b) -> a
fst = undefined

(+) :: Num a => a -> a -> a
(+) = undefined
