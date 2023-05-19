{-# LANGUAGE TypeApplications #-}
module Chapter18 () where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



trigger :: m (a, b, c)
trigger = undefined


---Chapter Exercises---

---1---

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

checkNope :: IO ()
checkNope = quickBatch $ monad (trigger @Nope @String @String @String)

---Exercise 2---

data PhhhbbtttEither b a = Left' a | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' f) (Left' a) = Left' (f a)
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' a) f = f a
  (>>=) (Right' b) _ = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(1, Left' <$> arbitrary)
                        ,(1, Right' <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

checkPhhhbbtttEither :: IO ()
checkPhhhbbtttEither = quickBatch $ monad (trigger @(PhhhbbtttEither String)
                                                   @String
                                                   @String
                                                   @String)

---Exercise 3---

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity . f $ a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity . f $ a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

checkIdentity :: IO ()
checkIdentity = quickBatch $ monad (trigger @(Identity) @String @String @String)

---Exercise 4---

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

---L'idea di Edoardo di scrivere applicative usando un'istanza di ---semigroup è molto bella

instance Semigroup (List a) where
  (<>) Nil xs = xs
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure = flip Cons Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil)
                        ,(1, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

checkList :: IO ()
checkList = quickBatch $ monad (trigger @(List) @String @String @String)

---Exercise 5---

j :: Monad m => m (m a) -> m a
j = (>>= id)

---Exercise 6---

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = f <$> ma

---Exercise 7---

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= \a -> mb >>= return . f a

---Exercise 8---

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf >>= \f -> ma >>= \x -> return $ f x

---Exercise 9---

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = flip meh'

meh' :: Monad m => (a -> m b) -> [a] -> m [b]
meh' f = foldr (l2 (:) . f) (pure [])

---Exercise 10---

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
