{-# LANGUAGE NoMonomorphismRestriction #-}
module Ch05.ChapterExercises where

-- Multiple choice
-- 1. A value of type [a] is a list whose elements are all of some type 𝑎
-- 2. A function of type [[a]] -> [a] could take a list of strings as an argument
-- 3. A function of type [a] -> Int -> a returns one element of type 𝑎 from a list
-- 4. A function of type (a, b) -> a takes a tuple argument and returns the first value

-- Determine the type
fn1 :: Num a => a
fn1 = (* 9) 6

fn2 :: Num a => (a, String)
fn2 = head [(0,"doge"),(1,"kitteh")]

fn3 :: (Integer, String)
fn3 = head [(0 :: Integer ,"doge"),(1,"kitteh")]

fn4 :: Bool
fn4 = if False then True else False

fn5 :: Int
fn5 = length [1, 2, 3, 4, 5]

fn6 :: Bool
fn6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

w1 :: Num a => a
x1 = 5
y1 = x1 + 5
w1 = y1 * 10

z2 :: Num a => a -> a
x2 = 5
y2 = x2 + 5
z2 y2 = y2 * 10

f3 :: Fractional a => a
x3 = 5
y3 = x3 + 5
f3 = 4 / y3

f4 :: String
x4 = "Julie"
y4 = " <3 "
z4 = "Haskell"
f4 = x4 ++ y4 ++ z4

-- Does it compile?
-- 1. do not compile. it could be fixed in this way:
bigNum x = (^) 5 $ x
wahoo = bigNum $ 10

-- 2. it compiles

-- 3. do not compile. it could be fixed in this way:
a1 = (+)
b = 5
c1 = a1 10
d = c1 200

-- 4. it compiles
a2 = 12 + b
b2 = 10000 * a2

-- Type variable or specific type constructor?
-- FPTV = fully polymorphic type variable
-- CPTV = constrained polymorphic type variable
-- CTC = concrete type constructor

-- f :: zed  -> Zed -> Blah
--      FPTV -> CTC -> CTC

-- f :: Enum b => a     -> b    -> C
--                FPTV  -> CPTV -> CTC

-- f :: f     -> g    -> C
--      FPTV  -> FPTV -> CTC

-- Write a type signature
functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b
-- given alpha equivalence c' and c'' are the same function

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r' :: [a] -> [a]
r a = a
r' a = tail a

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ anotherA = anotherA

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

-- Fix it
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- Type-Kwon-Do
-- 1.
f5 :: Int -> String
f5 = undefined
g5 :: String -> Char
g5 = undefined
h5 :: Int -> Char
h5 x = g5 $ f5 x

-- 2.
data A
data B
data C
q6 :: A -> B
q6 = undefined
w6 :: B -> C
w6 = undefined
e6 :: A -> C
e6 x = w6 $ q6 x

-- 3.
data X
data Y
data Z
xz7 :: X -> Z
xz7 = undefined
yz7 :: Y -> Z
yz7 = undefined
xform7 :: (X, Y) -> (Z, Z)
xform7 (x, y) = (xz7 x, yz7 y)

-- 4.
munge8 :: (x -> y)
  -> (y -> (w, z))
  -> x
  -> w
munge8 xy ywz x = fst $ ywz $ xy x
