module Chapter15 () where

import Test.QuickCheck

-------------------------
---Semigroup exercises---
-------------------------

---Exercise 1---
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  t <> s = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = quickCheck (semigroupAssoc :: TrivialAssoc)

---Exercise 2---
newtype Identity a = Identity a
  deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
   (Identity x) <> (Identity y) = Identity $ x <> y

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

checkIdentity :: IO ()
checkIdentity = quickCheck (semigroupAssoc :: IdentityAssoc String)

---Exercise 3---
data Two a b = Two a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

checkTwo :: IO ()
checkTwo = quickCheck (semigroupAssoc :: TwoAssoc String String)

---Exercise 4---
---Come renderlo pointwise?
data Three a b c = Three a b c
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c)
         => Semigroup (Three a b c) where
  (Three x1 x2 x3) <> (Three y1 y2 y3) =
     Three (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

checkThree :: IO ()
checkThree = quickCheck (semigroupAssoc :: ThreeAssoc String String String)
