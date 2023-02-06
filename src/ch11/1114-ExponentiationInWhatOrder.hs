module Ch11.ExponentiationInWhatOrder where

data Quantum = Yes | No | Both deriving (Eq, Show)

convert :: Quantum -> Bool
convert1 :: Quantum -> Bool
convert2 :: Quantum -> Bool
convert3 :: Quantum -> Bool
convert4 :: Quantum -> Bool
convert5 :: Quantum -> Bool
convert6 :: Quantum -> Bool
convert7 :: Quantum -> Bool
convert8 :: Quantum -> Bool

convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 Yes = False
convert2 No = False
convert2 Both = False

convert3 Yes = True
convert3 No = False
convert3 Both = False

convert4 Yes = False
convert4 No = True
convert4 Both = False

convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 Yes = True
convert6 No = True
convert6 Both = False

convert7 Yes = True
convert7 No = False
convert7 Both = True

convert8 Yes = False
convert8 No = True
convert8 Both = True
