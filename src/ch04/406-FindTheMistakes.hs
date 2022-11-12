module Ch04.FindTheMistakes where

-- not True && true
fn1 = not True && True

-- 2. not (x = 6)
fn2 x = not (x == 6)

-- 3. (1 * 2) > 5
fn3 = (1 * 2) > 5

-- 4. [Merry] > [Happy]
fn4 = ["Merry"] > ["Happy"]

-- 5. [1, 2, 3] ++ "look at me!"
fn5 = [1, 2, 3] ++ [4, 5, 6]
