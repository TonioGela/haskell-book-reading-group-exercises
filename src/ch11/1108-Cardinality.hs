module Ch11.Cardinality where

-- cardinality is 1
data PugType = PugData

-- c is 3
data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited

-- c of Int16
32768 + 32767 + 1 = 65536

-- What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
-- 2^8 = cardinality of Int8
-- 2^16 = cardinality of Int16
