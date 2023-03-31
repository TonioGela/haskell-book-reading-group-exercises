module Chapter15 () where

import Test.QuickCheck

----------------
---Semigroups---
----------------

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

---c'è un modo più elegante di scriverlo?
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

---C'è un modo di scriverlo in maniera tale da ottenere show inc = \x -> x + 1?
instance Show (Combine a b) where
  show _ = "just a function"

instance (Semigroup b) => Semigroup (Combine a b) where
  cf <> cg = Combine $ \a -> unCombine cf a <>  unCombine cg a
--coArbitrary perché [-, b] è controvariante?
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

combineAssoc :: (Eq b, Semigroup b) => a
                                       -> Combine a b
                                       -> Combine a b
                                       -> Combine a b
                                       -> Bool
combineAssoc a cf cg ch = (unCombine $ (cf <> cg) <> ch) a ==
                          (unCombine $ cf <> (cg <> ch)) a

type CombineAssoc a b = a
                        -> Combine a b
                        -> Combine a b
                        -> Combine a b
                        -> Bool

checkCombine :: IO ()
checkCombine = quickCheck (combineAssoc :: CombineAssoc String String)

---Exercise 10---

newtype Comp a = Comp { unComp :: a -> a }

instance Show (Comp a) where
  show _ = "just an endomorphism"

instance (Semigroup a) => Semigroup (Comp a) where
  cf <> cg = Comp $ \a -> unComp cf a <> unComp cg a

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

compAssoc :: (Eq a, Semigroup a) => a
                                    -> Comp a
                                    -> Comp a
                                    -> Comp a
                                    -> Bool
compAssoc a cf cg ch = (unComp $ (cf <> cg) <> ch) a ==
                       (unComp $ cf <> (cg <> ch)) a

type CompAssoc a = a -> Comp a -> Comp a -> Comp a -> Bool

checkComp :: IO ()
checkComp = quickCheck (compAssoc :: CompAssoc String)

---Exercise 11---
---TODO: Ho letto male gli esercizi e sono partito per la tangente wrappando il datatype, rifattorizzare come da libro.

data Validation a b = Failure' a | Success' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =  do
        a <- arbitrary
        b <- arbitrary
        elements [Failure' a, Success' b]

instance Semigroup (Validation a b) where
  (Failure' a) <> _ = Failure' a
  (Success' _) <> (Failure' a) = Failure' a
  (Success' b) <> (Success' _) = Success' b

type ValidationAssoc a b = Validation a b
                           -> Validation a b
                           -> Validation a b
                           -> Bool
checkValidation :: IO ()
checkValidation = quickCheck (semigroupAssoc :: ValidationAssoc String String)


---Exercise 12---
newtype AccumulateRight a b = AccumulateRight (Validation a b)
  deriving (Eq, Show)

---Non mi piace, troppo boilerplate
instance (Semigroup b) => Semigroup (AccumulateRight a b) where
   AccumulateRight (Failure' a) <> _ = AccumulateRight . Failure' $ a
   AccumulateRight (Success' _) <> AccumulateRight(Failure' a)
     = AccumulateRight . Failure' $ a
   AccumulateRight (Success' x) <> AccumulateRight (Success' y)
     = AccumulateRight . Success' $ (x <> y)


instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = AccumulateRight <$> (arbitrary :: Gen (Validation a b))

type AccumulateRightAssoc a b = AccumulateRight a b
                                -> AccumulateRight a b
                                -> AccumulateRight a b
                                -> Bool

checkAccumulateRight :: IO ()
checkAccumulateRight = quickCheck
  (semigroupAssoc :: AccumulateRightAssoc String String)

---Exercise 13---
---Provo una cosa diversa---
newtype AccumulateBoth a b = AccumulateBoth {unAccumulate :: Validation a b}
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  vf <> vs = case (unAccumulate vf, unAccumulate vs) of
               (Failure' _, Success' _) -> vf
               (Success' _, Failure' _) -> vs
               (Failure' x, Failure' y) -> AccumulateBoth . Failure' $ (x <> y)
               (Success' x, Success' y) -> AccumulateBoth . Success' $ (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = AccumulateBoth <$> (arbitrary :: Gen (Validation a b))

type AccumulateBothAssoc a b = AccumulateBoth a b
                              -> AccumulateBoth a b
                              -> AccumulateBoth a b
                              -> Bool

checkAccumulateBoth :: IO ()
checkAccumulateBoth = quickCheck
  (semigroupAssoc :: AccumulateBothAssoc String String)

-------------
---Monoids---
-------------
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

---Exercise 1---

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

checkTrivialMon :: IO ()
checkTrivialMon = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

---Exercise 2---

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

checkIdentityMon :: IO ()
checkIdentityMon = do
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

---Exercise 3---

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

checkTwoMon :: IO ()
checkTwoMon = do
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

---Exercise 4-5---
---True e False sono gli elementi neutri.

---Exercise 6---

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

monoidLeftIdentityCombine :: (Eq b, Monoid b) => a -> Combine a b -> Bool
monoidLeftIdentityCombine a cf = unCombine (mempty <> cf) a == unCombine cf a

monoidRightIdentityCombine :: (Eq b, Monoid b) => a -> Combine a b -> Bool
monoidRightIdentityCombine a cf = unCombine (cf <> mempty) a == unCombine cf a

checkCombineMon :: IO ()
checkCombineMon = do
  quickCheck (monoidLeftIdentityCombine :: String
                                           -> Combine String String
                                           -> Bool)
  quickCheck (monoidRightIdentityCombine :: String
                                            -> Combine String String
                                            -> Bool)

---Exercise 7---
---Come sopra---

---Exercise 8---
newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Show (Mem s a) where
  show _ = "just a state transition"

---fare un po' di golf coding giusto per prendere la mano?
instance Semigroup a => Semigroup (Mem s a) where
  sf <> sg = Mem $ \s -> ((fst . runMem sf $ s) <> (fst . runMem sg $ s)
                         , snd . runMem sf . snd . runMem sg $ s)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

---Come riutilizzare le istanze che ho scritto per Combine a b ?
memAssoc :: (Eq a, Eq s, Semigroup a) => s
                                         -> Mem s a
                                         -> Mem s a
                                         -> Mem s a
                                         -> Bool
memAssoc s cf cg ch = (runMem $ (cf <> cg) <> ch) s ==
                      (runMem $ cf <> (cg <> ch)) s

type MemAssoc s a = s
                    -> Mem s a
                    -> Mem s a
                    -> Mem s a
                    -> Bool

monoidLeftIdentityMem :: (Eq a, Eq s, Monoid a) => s -> Mem s a -> Bool
monoidLeftIdentityMem s cf = runMem (mempty <> cf) s == runMem cf s

monoidRightIdentityMem :: (Eq a, Eq s, Monoid a) => s -> Mem s a -> Bool
monoidRightIdentityMem s cf = runMem (cf <> mempty) s == runMem cf s

checkMem :: IO ()
checkMem = do
  quickCheck (memAssoc :: MemAssoc String String)
  quickCheck (monoidLeftIdentityMem :: String
                                       -> Mem String String
                                       -> Bool)
  quickCheck (monoidRightIdentityMem :: String
                                        -> Mem String String
                                        -> Bool)

