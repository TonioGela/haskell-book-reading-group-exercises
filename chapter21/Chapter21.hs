{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter21 () where

import Data.String (IsString)
import Test.QuickCheck (Arbitrary (arbitrary), frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (traversable)

trigger :: t (f a, g b, c, m)
trigger = undefined

--------------------------------------------------------------------------------
---Chapter Exercises------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
---Identity---------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
  foldr f b (Identity a) = f a b

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

checkIdentity :: IO ()
checkIdentity =
  quickBatch $
    traversable
      (trigger @Identity @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Constant---------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ b _ = b

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

checkConstant :: IO ()
checkConstant =
  quickBatch $
    traversable
      (trigger @(Constant String) @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Optional---------------------------------------------------------------------
--------------------------------------------------------------------------------

data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Yep f <*> Yep a = Yep (f a)

instance Foldable Optional where
  foldr _ b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

checkOptional :: IO ()
checkOptional =
  quickBatch $
    traversable
      (trigger @Optional @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---List-------------------------------------------------------------------------
--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Semigroup (List a) where
  (<>) Nil xs = xs
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure = flip Cons Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons x xs) = f x (foldr f b xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (3, Cons <$> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

checkList :: IO ()
checkList =
  quickBatch $
    traversable
      (trigger @List @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Three------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f b (Three _ _ c) = f c b

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c)
  where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

checkThree :: IO ()
checkThree =
  quickBatch $
    traversable
      (trigger @(Three String String) @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Pair-------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f b (Pair _ b') = f b' b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

checkPair :: IO ()
checkPair =
  quickBatch $
    traversable
      (trigger @(Pair String) @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Big--------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Big a b = Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldr f b (Big _ a b') = f b' $ f a b

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

checkBig :: IO ()
checkBig =
  quickBatch $
    traversable
      (trigger @(Big String) @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Bigger-----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Bigger a b = Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldr f b (Bigger _ a b' b'') = f b'' $ f b' $ f a b

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

checkBigger :: IO ()
checkBigger =
  quickBatch $
    traversable
      (trigger @(Bigger String) @[] @String @[] @String @String @String)

--------------------------------------------------------------------------------
---Other Exercises---------------------------------------------------------------
--------------------------------------------------------------------------------

data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Applicative n => Applicative (S n) where
  pure a = S (pure a) a
  S naf af <*> S na a = S (naf <*> na) (af a)

instance Foldable n => Foldable (S n) where
  foldr f b (S na a) = f a $ foldr f b na

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

--------------------------------------------------------------------------------
---Tree-------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Empty),
        (1, Leaf <$> arbitrary),
        (2, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

checkTree :: IO ()
checkTree =
  quickBatch $
    traversable
      (trigger @Tree @[] @String @[] @String @String @String)

{-
data Phantom m b = Phantom {unPhantom :: m}

instance Functor (Phantom m) where
    fmap _ (Phantom m) = Phantom m

instance Monoid m => Applicative (Phantom m) where
    pure _ = Phantom mempty
    Phantom m' <*> Phantom m = Phantom (m' <> m)

foldMap1 :: Monoid m => (a -> m) -> t a -> m
foldMap1 f = unPhantom . traverse (Phantom <$> f)
-}

--------------------------------------------------------------------------------
---Paolino's Homework-----------------------------------------------------------
--------------------------------------------------------------------------------

newtype OptionName = OptionName String
  deriving (Show, Eq, Ord, IsString)

newtype OptionValue = OptionValue {optionValue :: String}
  deriving (Show, Eq, IsString)

data Parser a
  = Pure a
  | forall b.
    Option
      { name :: OptionName,
        patch :: OptionValue -> Maybe (b -> a),
        rest :: Parser b
      }

-- example: clean --input foo --output bar --verbose true
-- example: clean --input foo --verbose false --output bar
data Clean = Clean
  { input :: String,
    output :: String,
    verbose :: Bool
  }
  deriving (Show, Eq)

test1 :: Bool
test1 =
  interpret
    parseClean
    [ ("--input", "foo"),
      ("--output", "bar"),
      ("--verbose", "true")
    ]
    == Just (Clean "foo" "bar" True)

test2 :: Bool
test2 =
  interpret
    parseClean
    [ ("--input", "foo"),
      ("--verbose", "false"),
      ("--output", "bar")
    ]
    == Just (Clean "foo" "bar" False)

type Args = [(OptionName, OptionValue)]

interpret :: Parser a -> Args -> Maybe a
interpret (Pure x) _xs = Just x
interpret Option {..} xs = do
  case lookup name xs of
    Nothing -> Nothing -- option name not found
    Just x -> do
      -- option name found
      f <- patch x
      x' <- interpret rest xs
      return $ f x'

parseClean :: Parser Clean
parseClean = Clean <$> parseInput <*> parseOutput <*> parseVerbose

mkOption :: OptionName -> (OptionValue -> Maybe b) -> Parser b
mkOption name patch = Option name (fmap (fmap const) patch) $ Pure ()

parseInput :: Parser String
parseInput = mkOption "--input" $ Just . optionValue

parseOutput :: Parser String
parseOutput = mkOption "--output" $ Just . optionValue

parseVerbose :: Parser Bool
parseVerbose = mkOption "--verbose" $ \case
  "true" -> Just True
  "false" -> Just False
  _ -> Nothing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Pure a) = Pure . f $ a
  fmap f Option {..} = Option name (fmap (fmap (f .)) patch) rest

-- l'fmap più interno è quello di Maybe, quello più esterno è quello del hom covariante, come anche il f .

instance Applicative Parser where
  pure :: a -> Parser a
  pure = Pure
  (<*>) :: Parser (b -> a) -> Parser b -> Parser a
  (<*>) (Pure f) pb = fmap f pb
  (<*>) Option {..} pb =
    Option name (fmap (fmap uncurry) patch) $ (,) <$> rest <*> pb

