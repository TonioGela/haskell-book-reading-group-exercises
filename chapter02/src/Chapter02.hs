module Chapter02(waxOn, triple, waxOnWhere, waxOff) where

z :: Integer
z = 7

x :: Integer
x = y ^ 2

waxOn :: Integer
waxOn = x * 5

y :: Integer
y = z + 8

triple :: Num a => a -> a
triple xx = xx * 3

waxOnWhere :: Integer
waxOnWhere = xx * 5 where
    xx = yy ^ 2
    yy = zz + 8
    zz = 7

waxOff :: Num a => a -> a
waxOff xx = triple xx