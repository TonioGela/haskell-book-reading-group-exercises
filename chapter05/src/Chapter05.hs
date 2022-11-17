module Chapter05 (module Chapter05) where

-- Given a type write the function

-- identity
i :: a -> a
i v = v

c :: a -> b -> a
c v _ = v

c'' :: b -> a -> b
c'' b _ = b

c' :: a -> b -> b
c' _ b = b

r :: [a] -> [a]
r = reverse

r' :: [a] -> [a]
r' = tail

co :: (b -> c) -> (a -> b) -> (a -> c)
co bToC aToB = bToC . aToB

co' :: (b -> c) -> (a -> b) -> (a -> c)
co' bToC aToB = \v -> bToC $ aToB v

a :: (a -> c) -> a -> a
a _ v = v

a' :: (a -> c) -> a -> c
a' ff = ff

-- Type-Kwon-Do

-- 1
------------------
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
-------------------
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
-------------------
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4
-------------------
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst $ ywz . xy $ x