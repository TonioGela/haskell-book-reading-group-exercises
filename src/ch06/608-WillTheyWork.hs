module Ch06.WillTheyWork where

-- 1. compile, returns 5
fn1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

-- 2. compile, returns LT
fn2 = compare (3 * 4) (3 * 5)

-- 3. does not compile, both arguments of compare function must have the same type
-- fixed
fn3 = compare "Julie" "AnotherString"

-- compile
fn4 = (5 + 3) > (3 + 6)