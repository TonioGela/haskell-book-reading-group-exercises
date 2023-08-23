import Control.Applicative
import Control.Monad
import Data.Functor
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope = do
  quickBatch (functor (undefined :: Nope (String, Char, Int)))
  quickBatch (applicative (undefined :: Nope (String, Char, Int)))
  quickBatch (monad (undefined :: Nope (String, Char, Int)))

--

data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Left' a, Right' b]

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' a) (Left' a') = Left' $ a a'
  (<*>) (Right' a) _ = Right' a
  (<*>) _ (Right' a) = Right' a

instance Monad (PhhhbbtttEither b) where
  return = Left'
  (>>=) (Left' a) f = f a
  (>>=) (Right' b) _ = Right' b
  
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

testPhhhbbtttEither = do
  quickBatch (functor (undefined :: PhhhbbtttEither (String, Char, Int) (Int, [String], Char) ))
  quickBatch (applicative (undefined :: PhhhbbtttEither (String, Char, Int) (Int, [String], Char) ))
  quickBatch (monad (undefined :: PhhhbbtttEither (String, Char, Int) (Int, [String], Char) ))

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) a = f <$> a

instance Monad Identity where
  return = pure
  -- perché con questa implementazione non funzionano i test?
  -- (>>=) ma f = join (fmap f ma)
  (>>=) (Identity a) f = (f a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

testIdentity = do
  quickBatch (functor (undefined :: Identity (String, Char, Int)))
  quickBatch (applicative (undefined :: Identity (String, Char, Int)))
  quickBatch (monad (undefined :: Identity (String, Char, Int)))

-- 


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [
    (1, return Nil),
    (1, do
      a <- arbitrary
      b <- arbitrary
      return $ Cons a b
    )]

instance Functor List where
  fmap _ (Nil) = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Semigroup (List a) where
  Nil <> a = a
  (Cons a b) <> r = Cons a (b <> r)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) ass@(Cons a as) = Cons (f a) ((f <$> as) <> (fs <*> ass))
  
instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = (f a) <> (join (f <$> as))

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

testList = do
  quickBatch (functor (undefined :: List (String, Char, Int)))
  quickBatch (applicative (undefined :: List (String, Char, Int)))
  quickBatch (monad (undefined :: List (String, Char, Int)))

testAll = do
  testNope
  testPhhhbbtttEither
  testIdentity
  testList

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 atob ma = atob <$> ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 atobtoc ma mb = ma >>= (\a -> (atobtoc a) <$> mb)

a :: Monad m => m a -> m (a -> b) -> m b
a ma matob = matob >>= (\x -> x <$> ma)
