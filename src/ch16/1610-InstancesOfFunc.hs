import Test.QuickCheck
import Test.QuickCheck.Function

testId :: (Functor f, Eq (f a)) => f a -> Bool
testId f  = fmap id f == f

testCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
testCompose func (Fun _ f) (Fun _ g) = (fmap (g . f) func) == ((fmap g) . (fmap f)) func

testSum :: Integer -> Integer -> Integer 
testSum a b = a + b

testDiv :: Integral a => a -> a -> a
testDiv a b = div a b

-- Identity
newtype Identity a = Identity a

instance (Eq a) => Eq (Identity a) where
  (==) (Identity a) (Identity b) = a == b

instance (Show a) => Show (Identity a) where
  show (Identity a) = show $ "Identity: " ++ show a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

-- Pair
data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

instance (Show a) => Show (Pair a) where
  show (Pair a b) = "Pair of " ++ show a ++ " and " ++ show b

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance Functor (Pair) where
  fmap f (Pair a b) = (Pair (f a) (f b))

-- Two
data Two a b = Two a b

instance (Eq a, Eq b) => Eq (Two a b) where
  (==) (Two a b) (Two c d) = a == c && b == d

instance (Show a, Show b) => Show (Two a b) where
  show (Two a b) = "Two: " ++ show a ++ ", " ++ show b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two x) where
  fmap :: (a -> b) -> (Two x a) -> (Two x b)
  fmap f (Two a b) = Two a (f b)

-- Three
data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- Three'
data Three' a b = Three' a b b deriving (Show)

instance (Eq a, Eq b) => Eq (Three' a b) where
  (==) (Three' a b c) (Three' d e f) = a == d && b == e && c == f

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

-- Four
data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- Four'
data Four' a b = Four' a a a b deriving (Show)

instance (Eq a, Eq b) => Eq (Four' a b) where
  (==) (Four' a b c d) (Four' e f g h) = a == e && b == f && c == g && d == h

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d) 

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- Trivial
data Trivial = Trivial deriving (Show, Eq)

instance Arbitrary Trivial where
  arbitrary = do
    return (Trivial)

-- instance of Functor for Trivial is impossible because Trivial data type constructor
-- has kind *. A type constructor with kind * -> * is required

main = do
  -- quickCheck (testId :: (Maybe Int) -> Bool)
  -- quickCheck (testCompose :: (Maybe Int) -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  
  verboseCheck (testId :: (Identity String) -> Bool)
  verboseCheck (testCompose :: (Identity Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)
  
  verboseCheck (testId :: (Pair Integer) -> Bool)
  verboseCheck (testCompose :: (Pair Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)
  
  verboseCheck (testId :: (Two Char Integer) -> Bool)
  verboseCheck (testCompose :: (Two Char Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)

  verboseCheck (testId :: (Three String Char Integer) -> Bool)
  verboseCheck (testCompose :: (Three String Char Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)

  verboseCheck (testId :: (Three' String Char) -> Bool)
  verboseCheck (testCompose :: (Three' String Char) -> (Fun Char Char) -> (Fun Char Char) -> Bool)

  verboseCheck (testId :: (Four String String Char Integer) -> Bool)
  verboseCheck (testCompose :: (Four String String Char Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)

  verboseCheck (testId :: (Four' String Integer) -> Bool)
  verboseCheck (testCompose :: (Four' String Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)
