{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Chapter16 () where

import Test.QuickCheck
import Control.Arrow ()


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == (fmap (g . f) x)


functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x)
                                      == (fmap g . fmap f $ x)

---Exercises---

newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity . f $ a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

checkIdentity :: IO ()
checkIdentity = do
  quickCheck (functorIdentity @Identity @String)
  quickCheck (functorCompose' @Identity @String @Integer @Integer)

data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

checkPair :: IO ()
checkPair = do
  quickCheck (functorIdentity @Pair @String)
  quickCheck (functorCompose' @Pair @String @Integer @Integer)

---Qui avremmo bisogno di un bifunctor---

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

checkTwo :: IO ()
checkTwo = do
  quickCheck (functorIdentity @(Two Integer) @Integer)
  quickCheck (functorCompose' @(Two Integer) @Integer @Integer @Integer)

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

checkThree :: IO ()
checkThree = do
  quickCheck (functorIdentity @(Three' String) @String)
  quickCheck (functorCompose' @(Three' Integer) @Integer @String @String)
---Gli altri sono più o meno la stessa solfa.
---Trivial non puo' essere un funtore, visto che il suo kind è *

------------------------
---Maybe is a functor---
------------------------

data Possibly a = LolNope | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers . f $ a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (1, Yeppers <$> arbitrary)]

checkPossibly :: IO ()
checkPossibly = do
  quickCheck (functorIdentity @Possibly @String)
  quickCheck (functorCompose' @Possibly @Integer @String @String)

----------------------------------------------
---Either is a functor (a bifunctor indeed)---
----------------------------------------------

data Sum a b = First a | Second b
  deriving (Show, Eq)

instance Functor (Sum a) where
  fmap f (Second b) = Second . f $ b
  fmap _ (First a) = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (1, Second <$> arbitrary)]

---Tutte ste ripetizioni potrebbero essere evitate se potessi passare tipi a ---funzioni
checkSum :: IO ()
checkSum = do
  quickCheck (functorIdentity @(Sum String) @String)
  quickCheck (functorCompose' @(Sum String) @String @String @String)

-----------------------
---Chapter Exercises---
-----------------------

---Exercises 1-2-3-4-5
--1: No
--2-3: Si'
--4: No; il kind è Mu :: (* -> *) -> *
--5: No, il kind è D :: *

---Rearrange the arguments to the type constructor of the datatype so the ---Functor instance works---
--1: Sum b a
--2: Company a c b
--3: More b a

---Write Functor instances for the following datatypes---

---1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor . f $ b

---2
---Chiedere a Paolino.
data K a b = K a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance Functor (K a) where
  fmap :: (a2 -> b) -> K a1 a2 -> K a1 b
  fmap _ (K a) = K a

checkKa :: IO ()
checkKa = do
  quickCheck (functorIdentity @(K String) @String)
  quickCheck (functorCompose' @(K String) @String @String @Integer)

{-
myValue :: K Int Bool
myValue = K 42

fn :: Bool -> [Bool]
fn b = [b]
-}
---3:
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

{-
myFlipValue :: Flip K Bool Int
myFlipValue = Flip  myValue

flipFn :: Int -> [Int]
flipFn n = [n]
-}

instance Functor (Flip K a) where
  fmap :: (a2 -> b) -> Flip K a1 a2 -> Flip K a1 b
  fmap f (Flip (K a)) = Flip . K . f $ a

instance Arbitrary (f b a) => Arbitrary (Flip f a b) where
  arbitrary = Flip <$> arbitrary

checkFlipK :: IO ()
checkFlipK = do
  quickCheck (functorIdentity @(Flip K String) @String)
  quickCheck (functorCompose' @(Flip K String) @String @String @String)

---5
newtype LiftItOut f a = LiftItOut (f a)
  deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
  fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut fa) = LiftItOut . fmap f $ fa

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

checkLiftOut :: IO ()
checkLiftOut = do
  quickCheck (functorIdentity @(LiftItOut []) @String)
  quickCheck (functorCompose' @(LiftItOut []) @String @Integer @Integer)

---6
data Parappa f g a = DaWrappa (f a) (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

checkParappa :: IO ()
checkParappa = do
  quickCheck (functorIdentity @(Parappa [] Maybe) @String)
  quickCheck (functorCompose' @(Parappa [] Maybe) @String @String @Integer)

---7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
  deriving (Show, Eq)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa ga) = IgnoringSomething fa (fmap h ga)

instance (Arbitrary (f a), Arbitrary (g b))
  => Arbitrary (IgnoreOne f g a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

checkIgnoreOne :: IO ()
checkIgnoreOne =  do
  quickCheck (functorIdentity @(IgnoreOne [] Maybe String) @String)
  quickCheck (functorCompose' @(IgnoreOne [] Maybe String)
                              @String
                              @String
                              @String)
---8
data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving (Show, Eq)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t))
  => Arbitrary (Notorious g o a t) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary
checkNotorious :: IO ()
checkNotorious = do
  quickCheck (functorIdentity @(Notorious [] String String) @String)
  quickCheck (functorCompose' @(Notorious [] String String)
                              @String
                              @String
                              @String)
---9

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =  frequency [(1, pure Nil)
                         , (3, Cons <$> arbitrary <*> arbitrary)]

checkList :: IO ()
checkList = do
  quickCheck (functorIdentity @List @Integer)
  quickCheck (functorCompose' @List @String @String @String)

---10

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats l c r) = MoreGoats (fmap f l) (fmap f c) (fmap f r)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = frequency [(2, pure NoGoat)
                        , (1, OneGoat <$> arbitrary)
                        , (1, MoreGoats <$> arbitrary
                                        <*> arbitrary
                                        <*> arbitrary)]

checkGoat :: IO ()
checkGoat = do
  quickCheck (functorIdentity @GoatLord @Integer)
  quickCheck (functorCompose' @GoatLord @String @String @String)

---11

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print" ++ s ++ (show a)
  show (Read _) = "Read" ++ "a function"

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = frequency [(1, pure Halt)
                        , (2, Print <$> arbitrary <*> arbitrary)
                        , (1, Read <$> arbitrary)]

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print st a) = Print st . f $ a
  fmap f (Read h) = Read $ f . h

functorCompose'' :: Eq c => String -> TalkToMe a  -> Fun a b -> Fun b c -> Bool
functorCompose'' st x (Fun _ f) (Fun _ g) =
  case ((fmap (g . f) x), (fmap g . fmap f $ x)) of
    (Halt, Halt) -> True
    (Print s1 a1, Print s2 a2) -> (s1 == s2) && (a1 == a2)
    (Read f1, Read f2) -> f1 st == f2 st
    _ -> False

checkTalkToMe :: IO ()
checkTalkToMe = quickCheck (functorCompose'' @String @String @String)




data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

data TreeL a = LeafL a | NodeL (TreeL a) (TreeL a)

instance Functor TreeL where
    fmap :: (a -> b) -> TreeL a -> TreeL b
    fmap f (LeafL a) = LeafL (f a)
    fmap f (NodeL l r) = NodeL (fmap f l) (fmap f r)


data Trie a = Trie [(a, Trie a)]
  deriving (Show, Eq)

{-
instance Functor Trie where
    fmap f (Trie xs) = Trie $ fmap (f (***) fmap f) xs
-}


newtype F a = F (Int -> (a,a))

instance Functor F where
    fmap f (F g) = F $ \x -> let (a,b) = g x in (f a, f b)



newtype C a = C ((a -> Int) -> Int) -- si puo'

runC :: C Int -> Int
runC (C f) = f id

instance Applicative C where
    pure a = C $ \h -> h a
    (C f) <*> (C g) = C $ \h -> f $ \a -> g $ h . a

instance Functor C where
    fmap :: (a -> b) -> C a -> C b
    fmap f (C g) = C $ \h -> g $ h . f

--- Dimostrazione che C è un funtore

--- fmap id (C g) = C $ \h -> g $ (h . id)
            ---   = C $ \h -> g h
            ---   = C g

--- fmap f . fmap l (C g) = fmap f $ (C \h -> g $ (h . l))
 ---                      = C \h' -> \h -> g $ (h . l) $ h' . f  (definizione di $)
 ---                      = C \h -> g (h . f . l)
 ---                      = C \h -> g (h . (f . l))
 ---                      = fmap (f . l) (C g)
