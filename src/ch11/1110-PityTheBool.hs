module Ch11.PityTheBool where

data BigSmall =
  Big Bool | Small Bool deriving (Eq, Show)
-- BigSmall cardinality is 4

data NumberOrBool =
  Numba Int8 | BoolyBool Bool deriving (Eq, Show)
-- NumberOrBool cardinality is 512
