module Ch17.Constant where

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

-- :t Constant :: a -> Constant a b
instance Monoid a => Applicative (Constant a) where
  -- pure :: a -> Constant a b
  pure = Constant $ mempty
  -- Constant a (b -> c) -> Constant a b -> Constant a c
  (<*>) (Constant a) (Constant a') = Constant (mappend a a')
