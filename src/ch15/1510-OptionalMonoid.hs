module Ch15.OptionalMonoid where

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada
  mappend a Nada = a
  mappend Nada b = b
  mappend (Only a) (Only b) = Only (mappend a b)

instance (Semigroup a) => Semigroup (Optional a) where
  a <> Nada = a
  Nada <> b = b
  (Only a) <> (Only b) = Only (a <> b)

