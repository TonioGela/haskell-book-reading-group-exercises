module Ch02.ChapterExercises where

-- Parenthesization
-- 1. 2 + 2 * 3 - 1
-- 1. 2 + (2 * 3) - 1

-- 2. (^) 10 $ 1 + 1
-- 2. (^) 10 $ (1 + 1)

-- 3. 2 ^ 2 * 4 ^ 5 + 1
-- 3. (2 ^ 2) * (4 ^ 5) + 1

-- Equivalent expressions
-- 1. yes
-- 2. yes
-- 3. no
-- 4. no
-- 5. no

z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8

fn1 = 10 + waxOn
fn2 = (+10) waxOn
fn3 = (-) 15 waxOn
fn4 = (-) waxOn 15

triple x = x * 3

waxOnWithWhere = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

waxOff x = triple x
