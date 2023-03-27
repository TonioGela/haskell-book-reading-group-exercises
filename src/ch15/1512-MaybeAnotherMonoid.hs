module Ch15.MaybeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck
import Control.Monad

type S = String
type B = Bool

-- quickCheck (monoidAssoc :: S -> S -> S -> B)
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> b) <> c == a <> (b <> c)

-- quickCheck (monoidLeftId :: S -> B)
monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = mempty <> a == a

-- quickCheck (monoidRightId :: S -> B)
monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = a <> mempty == a

data Bull = Fools | Twoo deriving (Eq, Show)
instance Arbitrary Bull where
  arbitrary = 
    frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools


instance Semigroup Bull where
  (<>) _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main1 :: IO ()
main1 = do
  let ma = monoidAssoc
      mli = monoidLeftId
      mri = monoidRightId
  quickCheck (ma :: Bull -> Bull -> Bull -> Bool)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)

--
-- Exercise: Maybe Another Monoid

data Optional a = Only a | Nada deriving (Eq, Show)
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' {getFirst' = Nada}
  mappend (First' {getFirst' = Nada}) b = b
  mappend a _ = a

instance Semigroup (First' a) where
  (<>) (First' {getFirst' = Nada}) b = b
  (<>) a _ = a

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

instance Arbitrary (First' a) where
  arbitrary =
    frequency [
      (1, return (First' {getFirst' = Nada}))
      (1, return (First' {getFirst' = Only a}))
    ]

main2 :: IO ()
main2 = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftId :: FstId)
  quickCheck (monoidRightId :: FstId)
