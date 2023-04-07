{-# LANGUAGE TypeApplications #-}
module Chapter17 () where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---Utilities---
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n-1) as)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons a xs) (Cons b ys) = Cons (f a b) (zipWith' f xs ys)

trigger :: f (a, b, c)
trigger = undefined

---List Applicative---

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) $ fmap f l

instance Applicative List where
  pure a = Cons a Nil
  (<*>) fs xs = flatMap (<$> xs) fs
  ---flatMap $ \a -> fmap (flip ($) a)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =  frequency [(1, pure Nil)
                         , (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where (=-=) = eq
checkList :: IO ()
checkList = quickBatch $ applicative
                         (trigger @List @String @String @String)

---ZipList Applicative---
newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
---All'inizio pensavo pure fosse semplicement ZipList' . pure, ma questa
---versione non rispetta (tra le altre cose) pure id <*> xs == xs per liste
---con lunghezza maggiore di 1

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipWith' ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l


checkZipList :: IO ()
checkZipList = quickBatch $ applicative
                            (trigger @ZipList'
                                     @String
                                     @Char
                                     @String)
