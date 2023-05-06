{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Test.QuickCheck

testId :: (Functor f, Eq (f a)) => f a -> Bool
testId f  = fmap id f == f

testCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
testCompose func (Fun _ f) (Fun _ g) = (fmap (g . f) func) == ((fmap g) . (fmap f)) func

-- cannot write instance of Functor for Bool

data BoolAndSomethingElse a = False' a | True' a
instance (Eq a) => Eq (BoolAndSomethingElse a)  where
  (==) (False' a) (False' b)  = a == b
  (==) (True' a)  (True' b)   = a == b
  (==) _          _           = False
instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [False' a, True' a]
instance (Show a) => Show (BoolAndSomethingElse a) where
  show (False' a) = "False': " ++ show a
  show (True' a) = "True': " ++ show a
instance Functor (BoolAndSomethingElse) where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)
testBoolAndSomethingElse = do
  verboseCheck (testId :: BoolAndSomethingElse String -> Bool)
  verboseCheck (testCompose :: BoolAndSomethingElse Int -> Fun Int Int -> Fun Int String -> Bool)

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)
instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ (Falsish) = Falsish
  fmap f (Truish a) = Truish (f a)
instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [Falsish, (Truish a)]
testBoolAndMaybeSomethingElse = do
  verboseCheck (testId :: BoolAndMaybeSomethingElse Int -> Bool)
  verboseCheck (testCompose :: BoolAndMaybeSomethingElse Int -> Fun Int Int -> Fun Int String -> Bool)

-- cannot write instance of Functor for newtype Mu
-- cannot write instance of Functor for data D

data D a = D (Array Word Word) Int a deriving (Eq, Show)
instance Functor (D) where
  fmap f (D aw i a) = D aw i (f a)

data Sum b a = First a | Second b
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

t1 = fmap (+1) (L 1 2 3)
t2 = fmap (+1) (R 1 2 3)

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]
testQuant = do
  verboseCheck (testId :: Quant Int Char -> Bool)
  verboseCheck (testCompose :: Quant Int Char -> Fun Char Char -> Fun Char Char -> Bool)
  
data K a b = K a deriving (Eq, Show)
instance Functor (K a) where
  fmap _ (K a) = K a
instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = fmap K arbitrary
testKab = do
  verboseCheck (testId :: K Char String -> Bool)
  verboseCheck (testCompose :: K Char String -> Fun String Int -> Fun Int Char -> Bool)

-- newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
-- newtype K' a b = K' a deriving (Eq, Show)
-- instance Arbitrary (a) => Arbitrary (K' a b) where
--   arbitrary = K' <$> arbitrary
-- instance Functor (Flip K' a) where
--   fmap f (Flip (K' a)) = (Flip (K' (f a)))
-- instance Arbitrary (f b a) => Arbitrary (Flip f a b) where
--   arbitrary = do
--     f <- arbitrary
--     return (Flip f)
-- testFlip = do
--   verboseCheck (testId :: Flip K' String Char -> Bool)
--   verboseCheck (testCompose :: Flip K' String Char -> Fun Char Int -> Fun Int String -> Bool)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
testEvilGoateeConst = do
  verboseCheck (testId      :: EvilGoateeConst Char Char -> Bool)
  verboseCheck (testCompose :: EvilGoateeConst Char Char -> Fun Char String -> Fun String Int -> Bool)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary
instance (Functor f) => Functor (LiftItOut f) where
  fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut (fa)) = LiftItOut $ f <$> fa
testLiftItOut = do
  verboseCheck (testId      :: LiftItOut ((,) Char) Char -> Bool)
  verboseCheck (testCompose :: LiftItOut ((,) Char) Char -> Fun Char String -> Fun String Int -> Bool)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)
instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = do
    fa <- arbitrary
    ga <- arbitrary
    return $ DaWrappa fa ga
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)
testParappa = do
  verboseCheck (testId      :: Parappa [] ((,) Char) Char -> Bool)
  verboseCheck (testCompose :: Parappa [] ((,) Char) Char -> Fun Char String -> Fun String Int -> Bool)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)
instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    fa <- arbitrary
    gb <- arbitrary
    return (IgnoringSomething fa gb)
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
testIgnoreOne = do
  verboseCheck (testId      :: IgnoreOne [] ((,) Char) Int String -> Bool)
  verboseCheck (testCompose :: IgnoreOne [] ((,) Char) Int String -> Fun String String -> Fun String Int -> Bool)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)
instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = do
    go <- arbitrary
    ga <- arbitrary
    gt <- arbitrary
    return (Notorious go ga gt)
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
testNotorious = do
  verboseCheck (testId      :: Notorious [] Int Char String -> Bool)
  verboseCheck (testCompose :: Notorious [] Int Char String -> Fun String String -> Fun String Int -> Bool)

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    elements [Nil, Cons a la]
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a c) = Cons (f a) (fmap f c)
testLista = do
  verboseCheck (testId :: List String -> Bool)
  verboseCheck (testCompose :: List String -> Fun String String -> Fun String Int -> Bool)
  
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)
instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    elements [NoGoat, OneGoat a, MoreGoats b c d]
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)
testGoatLord = do
  verboseCheck (testId :: GoatLord [Int] -> Bool)
  verboseCheck (testCompose :: GoatLord [Int] -> Fun [Int] String -> Fun String Int -> Bool)
  
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = frequency [(1, pure Halt)
                        , (2, Print <$> arbitrary <*> arbitrary)
                        , (1, Read <$> arbitrary)]
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read h) = Read (\x -> f (h x))

  