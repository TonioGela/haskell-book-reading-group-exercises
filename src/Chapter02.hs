module Chapter02 where

circleArea r = pi * square r

square x = x * x

-- Section 2.7 Declaring values
-- Excercises: Heal the sick

area x = 3.14 * (x * x)

-- double x = b * 2 -- b doesn't exist

x = 7

y = 10

f = x + y

-- Section 2.10 let and where
-- Excercises: A head code

mult1 = x * y
  where
    x = 5
    y = 6

-- let x = 3; y = 1000 in x * 3 + y
headCode1 = x * 3 + y
  where
    x = 3
    y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
headCode2 = x * 5
  where
    y = 10
    x = 10 * 5 + y

-- let x = 7; y = negate x; z = y * 10 in z / x + y
headCode3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10
