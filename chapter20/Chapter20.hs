{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Chapter20 () where

import Data.Monoid (Sum(..), Product(..), Any(..))

import Test.QuickCheck
import Test.QuickCheck.Checkers ( eq, quickBatch, EqProp(..))
import Test.QuickCheck.Classes (foldable)

---Direttamente da ---https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:foldMap
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (min . Just) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (const (const True)) False

length' :: (Foldable t) => t a -> Int
length' = foldr (const (+1)) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

---Chapter Exercises---

trigger :: t (a, b, m, n, o)
trigger = undefined

---1---

newtype Constant a b = Constant a
  deriving (Show, Eq)

instance Foldable (Constant a) where
  foldMap _ _ = mempty
  foldr _ z _ = z

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

checkConstant :: IO ()
checkConstant = quickBatch $ foldable
  (trigger @(Constant Int) @String @String @String @(Sum Int) @String)

---2---

data Two a b = Two a b
  deriving (Show, Eq)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
  foldr f z (Two _ b) = f b z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

checkTwo :: IO ()
checkTwo = quickBatch $ foldable
  (trigger @(Two Int) @String @String @String @(Sum Int) @String)

---3---

data Three a b c = Three a b c
  deriving (Show, Eq)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f z (Three _ _ c) = f c z

instance (Arbitrary a, Arbitrary b, Arbitrary c)
        => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

checkThree :: IO ()
checkThree = quickBatch $ foldable
  (trigger @(Three String String ) @String @String @String @(Sum Int) @String)

---4---

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2
  foldr f z (Three' _ b1 b2) = f b1 (f b2 z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

checkThree' :: IO ()
checkThree' = quickBatch $ foldable
  (trigger @(Three' String) @String @String @String @(Sum Int) @String)

---5---

data Four' a b = Four' a b b b
  deriving (Show, Eq)

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3
  foldr f z (Four' _ b1 b2 b3) = f b1 (f b2 (f b3 z))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

checkFour' :: IO ()
checkFour' = quickBatch $ foldable
  (trigger @(Four' String) @String @String @String @(Sum Int) @String)

---Filtering---

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
