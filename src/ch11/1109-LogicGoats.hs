module Ch11.LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

newtype NT1 = NT1 (Int, String)
instance TooMany NT1 where
  tooMany (NT1 (i, s)) = i > 42

newtype NT2 = NT2 (Int, Int)
instance TooMany NT2 where
  tooMany (NT2 (i1, i2)) = (i1 + i2) > 42

