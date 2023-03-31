module Ch15.ChapterExercises where

import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftId :: (Eq a, Monoid a) => a -> Bool
monoidLeftId a = mempty <> a == a

monoidRightId :: (Eq a, Monoid a) => a -> Bool
monoidRightId a = a <> mempty == a

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) a b = a

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 = verboseCheck (semigroupAssoc :: TrivAssoc)

-- 2
newtype Identity a = Identity a

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    b <- arbitrary
    return (Identity b)

instance (Eq a) => Eq (Identity a) where
  (==) (Identity a) (Identity b) = a == b

instance (Show a) => Show (Identity a) where
  show (Identity a) = "Identity " ++ show a

type IdAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

main2 :: IO ()
main2 = verboseCheck (semigroupAssoc :: IdAssoc)

-- 3
data Two a b = Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = (Two (a <> c) (b <> d))

instance (Show a, Show b) => Show (Two a b) where
  show (Two a b) = "Two ()" ++ show a ++ ", " ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (Two a b) where
  (Two a b) == (Two c d) = a == c && b == d

type TwoAssoc = Two String Any -> Two String Any -> Two String Any -> Bool

main3 :: IO ()
main3 = verboseCheck (semigroupAssoc :: TwoAssoc)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

type ThreeAssoc = (Three Any All [Integer]) -> (Three Any All [Integer]) -> (Three Any All [Integer]) -> Bool

main4 :: IO ()
main4 = verboseCheck (semigroupAssoc :: ThreeAssoc)

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

type FourAssoc = (Four Any [Float] [Integer] String) -> (Four Any [Float] [Integer] String) -> (Four Any [Float] [Integer] String) -> Bool

main5 :: IO ()
main5 = verboseCheck (semigroupAssoc :: FourAssoc)

--6
newtype BoolConj = BoolConj Bool deriving Show

instance Eq BoolConj where
  (==) (BoolConj a) (BoolConj b) = a == b

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

instance Semigroup Bool where
  (<>) True True  = True
  (<>) _ False    = False
  (<>) False _    = False

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a <> b)

main6 :: IO ()
main6 = verboseCheck (semigroupAssoc :: (BoolConj -> BoolConj -> BoolConj -> Bool))

-- 7
newtype Bool' = Bool' Bool deriving (Eq, Show) -- instance of Semigroup for Bool is already defined
newtype BoolDisj = BoolDisj Bool' deriving (Eq, Show)
instance Semigroup Bool' where
  (<>) (Bool' True) (Bool' _) = Bool' True
  (<>) (Bool' _) (Bool' True) = Bool' True
  (<>) (Bool' _) (Bool' _)    = Bool' False

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a <> b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj (Bool' a))

main7 :: IO ()
main7 = verboseCheck (semigroupAssoc :: (BoolDisj -> BoolDisj -> BoolDisj -> Bool))

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd a) (Snd b) = Snd a
  (<>) (Snd a) _ = Snd a
  (<>) _ (Snd b) = Snd b
  (<>) _ b = b
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

main8 :: IO ()
main8 = verboseCheck (semigroupAssoc :: ((Or Bool Integer) -> (Or Bool Integer) -> (Or Bool Integer) -> Bool))

-- TODO missing some exercises


-- monoids
-- 1
instance Monoid Trivial where
  mempty = Trivial

main12 :: IO ()
main12 = do
  verboseCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  verboseCheck (monoidLeftId :: Trivial -> Bool)
  verboseCheck (monoidRightId :: Trivial -> Bool)
  
-- 2
instance (Monoid a) => Monoid (Identity a) where
  mempty = (Identity mempty)

main13 :: IO ()
main13 = do
  verboseCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  verboseCheck (monoidLeftId :: Identity String -> Bool)
  verboseCheck (monoidRightId :: Identity String -> Bool)

-- 3
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two (mempty :: a) (mempty :: b)

main14 :: IO ()
main14 = do
  verboseCheck (semigroupAssoc :: Two String Any -> Two String Any -> Two String Any -> Bool)
  verboseCheck (monoidLeftId :: Two String Any -> Bool)
  verboseCheck (monoidRightId :: Two String Any -> Bool)

-- 4
instance Monoid Bool where
  mempty = True

instance Monoid BoolConj where
  mempty = (BoolConj mempty)

main15 :: IO ()
main15 = do
  verboseCheck (semigroupAssoc :: (BoolConj -> BoolConj -> BoolConj -> Bool))
  verboseCheck (monoidLeftId :: BoolConj -> Bool)
  verboseCheck (monoidRightId :: BoolConj -> Bool)

-- 5
instance Monoid Bool' where
  mempty = Bool' False

instance Monoid BoolDisj where
  mempty = BoolDisj mempty

main16 :: IO ()
main16 = do
  verboseCheck (semigroupAssoc :: (BoolDisj -> BoolDisj -> BoolDisj -> Bool))
  verboseCheck (monoidLeftId :: BoolDisj -> Bool)
  verboseCheck (monoidRightId :: BoolDisj -> Bool)

-- 6
-- TODO missing some exercises
