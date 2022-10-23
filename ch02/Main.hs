module Ch02 where

-- 2.5 Exercises: Comprehension check

p x = 3.14 * (x * x)

p' x = pi * (x * x)

-- 2.6 Exercises: Parentheses and association

-- 1. yes
-- 2. no
-- 3. yes

-- 2.7 Exercises: Heal the sick

area x = 3.14 * (x * x)

double x = x * 2

x = 7
y = 10
f=x+y

-- Exercises: A head code

-- let x = 5 in x -- 5
-- let x = 5 in x * x -- 25
-- let x = 5; y = 6 in x * y -- 30
-- let x = 3; y = 1000 in x + 3 -- 6

-- let x = 3; y = 1000 in x * 3 + y

h1 = x * 3 + y
  where
    x = 3
    y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
h2 = x * 5
  where
    y = 10
    x = 10 * 5 + y

{-
let x = 7
  y = negate x
  z = y * 10 
in z / x + y
-}
h3 = z / x + y
  where
    x = 7
    y = negate 7
    z = y * 10

-- 2.11 Chapter exercises

-- Parenthesization

p1 = 2 + (2 * 3) - 1
p1' = 2 + (2 * 3) - 1

p2 = (^) 10 $ 1 + 1
p2' = (^) 10 (1 + 1)

p3 = 2 ^ 2 * 4 ^ 5 + 1
p3' = (2 ^ 2) * (4 ^ 5) + 1

-- Equivalent expressions

{-
1. yes
2. yes
3. no
4. no
5. no
-}

-- More fun with functions

z'=7
y'=z'+8
x'=y'^2
waxOn = x' * 5 -- 1125

-- 10 + waxOn == 1135
-- (+1) + waxOn == 1135
-- (-) 15 waxOn = -1110
-- (-) waxOn 15 = 1110

triple x = x * 3

-- triple waxOn == 3375

waxOn' = x * 5
  where
    z = 7
    y = z + 8
    x = y^2

waxOff x = triple x
