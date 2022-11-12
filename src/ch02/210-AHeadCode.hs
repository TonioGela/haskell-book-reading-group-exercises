module Ch02.AHeadCode where
-- 1. let x = 5 in x
-- 5

-- 2. let x = 5 in x * x
-- 25

-- 3. let x = 5; y = 6 in x * y
-- 30

-- 4. let x = 3; y = 1000 in x + 3
-- 6

-- 1. let x = 3; y = 1000 in x * 3 + y
func1 = x * 3 + y
  where x = 3
        y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
func2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3. let x = 7
--        y = negate x
--        z = y * 10
--    in z / x + y
func3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
