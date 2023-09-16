module Ch22.Ask where

newtype Reader r a = Reader (r -> a)

ask :: Reader a a
ask = Reader (\a -> a)

ask' :: Reader a a
ask' = Reader id

-- a -> a is the id function
-- this type has infinite inhabitants

instance Functor (Reader r) where
  -- fmap (a -> b) -> (Reader r a) -> (Reader r b)
  --fmap f (Reader ra) = Reader (\r -> f (ra r))
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  -- <*> Reader (a -> b) -> Reader (a) -> Reader (b)
  (<*>) (Reader rab) (Reader ra) = Reader (\r -> (rab r) $ (ra r))
  pure a = Reader (\r -> a)

