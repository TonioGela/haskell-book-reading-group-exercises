
module Chapter15 () where

import Test.QuickCheck
-------------------------
---Semigroup exercises---
-------------------------

---Exercise 1---
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

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

---Exercise 5---
data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (Four a b c d) where
  (Four x1 x2 x3 x4) <> (Four y1 y2 y3 y4) =
    Four (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourAssoc a b c d = Four a b c d
                         -> Four a b c d
                         -> Four a b c d
                         -> Bool

checkFour :: IO ()
checkFour = quickCheck (semigroupAssoc :: FourAssoc String String String String)

---Exercise 6---
newtype BoolConj = BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj False) <> (BoolConj _) = BoolConj False
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj True) <> (BoolConj False) = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkConj :: IO ()
checkConj = quickCheck (semigroupAssoc :: BoolConjAssoc)

---Exercise 7---
newtype BoolDisj = BoolDisj Bool
  deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _) = BoolDisj True
  (BoolDisj False) <> (BoolDisj True) = BoolDisj True
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkDisj :: IO ()
checkDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)

---Exercise 8---

data Or a b = Fst a | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  (Fst _) <> (Fst y) = Fst y
  (Fst _) <> (Snd y) = Snd y

---c'è un modo più elegante di scriverlo
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary =  do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

checkOr :: IO ()
checkOr = quickCheck (semigroupAssoc :: OrAssoc String String)

---Exercise 9---

newtype Combine a b = Combine { unCombine :: a -> b}

instance Show (Combine a b) where
  show _ = "just a function"

instance (Semigroup b) => Semigroup (Combine a b) where
  cf <> cg = Combine $ \n -> unCombine cf n <>  unCombine cg n
--coArbitrary perché Hom (-, b) è controvariante?
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

compAssoc :: (Eq b, Semigroup b) => a
                                    -> Combine a b
                                    -> Combine a b
                                    -> Combine a b
                                    -> Bool
compAssoc a cf cg ch = (unCombine $ (cf <> cg) <> ch) a ==
                       (unCombine $ cf <> (cg <> ch)) a

type CombineAssoc a b = a -> Combine a b -> Combine a b -> Combine a b -> Bool

checkCombine :: IO ()
checkCombine = quickCheck (compAssoc :: CombineAssoc String String)


