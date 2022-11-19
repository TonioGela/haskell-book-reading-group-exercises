module Chapter06 () where

newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False

data Pair a = Pair a a

data Tuple a b = Tuple a b

data Which a = ThisOne a | ThatOne a

data EitherOr a b = Hello a | Goodbye b
