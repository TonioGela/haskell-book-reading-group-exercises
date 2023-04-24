{-# LANGUAGE TypeApplications #-}
module Chapter17 () where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---------------
---Utilities---
---------------

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

----------------------
---List Applicative---
----------------------

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

-------------------------
---ZipList Applicative---
-------------------------

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

---------------------
---Sum Applicative---
---------------------

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second a) = Second $ f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary)
                        ,(3, Second <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

checkSum :: IO ()
checkSum = quickBatch $ applicative
                        (trigger @(Sum String)
                                 @String
                                 @Char
                                 @String)
----------------------------
---Validation Applicative---
----------------------------

data Validation e a = Failure' e | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' e) (Failure' e') = Failure' $ e <> e'
  (<*>) (Failure' e) _ = Failure' e
  (<*>) _ (Failure' e) = Failure' e
  (<*>) (Success' f) (Success' a) = Success' $ f a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Failure' <$> arbitrary)
                        ,(3, Success' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

checkValidation :: IO ()
checkValidation = quickBatch $ applicative
                               (trigger @(Validation String)
                                        @String
                                        @Char
                                        @String)

-----------------------
---Chapter Exercises---
-----------------------

---1-4---
---Boh, non mi sembra che ci sia molto da dire su questi esercizi.

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq
checkIdentity :: IO ()
checkIdentity = quickBatch $ applicative
                             (trigger @(Identity)
                                      @String
                                      @Char
                                      @String)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where (=-=) = eq

checkPair :: IO ()
checkPair = quickBatch $ applicative
                         (trigger @(Pair)
                                  @String
                                  @Char
                                  @String)


data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' b) = Two (a <> a') $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

checkTwo :: IO ()
checkTwo = quickBatch $ applicative
                        (trigger @(Two String)
                                 @String
                                 @Char
                                 @String)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c)
        => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

checkThree :: IO ()
checkThree = quickBatch $ applicative
                           (trigger @(Three String String)
                                    @String
                                    @Char
                                    @String)

---L'istanza di applicative dei tipi rimanenti si implementa sulla falsariga
---delle precedenti, quindi non le riporto.

---L'esercizio che usa liftA3 l'ho fatto usando l'implementazione di liftA3
---anche se la soluzione diretta è molto più pulita.

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = (,,) <$> xs <*> ys <*> zs

-------------------------
---Esercizi di Paolino---
-------------------------

---1---
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure x = Node Leaf x Leaf
    (<*>) Leaf _ = Leaf
    (<*>) _ Leaf = Leaf
    (<*>) (Node l f r) (Node l' x r') = Node (l <*> l') (f x) (r <*> r')

--TODO: implementare Arbitrary per Tree (con un bound sulla profondità)
---2---

data TreeL a = LeafL a | NodeL (TreeL a) (TreeL a)

instance Functor TreeL where
    fmap f (LeafL a) = LeafL (f a)
    fmap f (NodeL l r) = NodeL (fmap f l) (fmap f r)

instance Applicative TreeL where
    pure = LeafL
    (<*>) (LeafL f) (LeafL x) = LeafL . f $ x
    (<*>) (LeafL f) (NodeL l r) = NodeL (fmap f l) (fmap f r)
    (<*>) (NodeL l r) (LeafL x) = NodeL (fmap ($ x) l) (fmap ($ x) r)
    (<*>) (NodeL l r) (NodeL l' r') = NodeL (l <*> l') (r <*> r')
