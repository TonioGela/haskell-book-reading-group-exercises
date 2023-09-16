{-# LANGUAGE InstanceSigs #-}
module Ch22.ReadingComprehension where

newtype Reader r a = Reader (r -> a)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fn fa fb = (pure fn <*> fa) <*> fb

myLiftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2' fn fa fb = fn <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  -- fmap (a -> b) -> (Reader r a) -> (Reader r b)
  --fmap f (Reader ra) = Reader (\r -> f (ra r))
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  -- <*> Reader (a -> b) -> Reader (a) -> Reader (b)
  (<*>) (Reader rab) (Reader ra) = Reader (\r -> (rab r) $ (ra r))
  pure a = Reader (\r -> a)

