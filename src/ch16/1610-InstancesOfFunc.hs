import Test.QuickCheck
import Test.QuickCheck.Function

testId :: (Functor f, Eq (f a)) => f a -> Bool
testId f  = fmap id f == f

testCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
testCompose func (Fun _ f) (Fun _ g) = (fmap (g . f) func) == ((fmap g) . (fmap f)) func

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

main = do
  -- quickCheck (testId :: (Maybe Int) -> Bool)
  -- quickCheck (testCompose :: (Maybe Int) -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  verboseCheck (testId :: (Identity String) -> Bool)
  verboseCheck (testCompose :: (Identity Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)
  verboseCheck (testId :: (Pair Integer) -> Bool)
  verboseCheck (testCompose :: (Pair Integer) -> (Fun Integer Integer) -> (Fun Integer Integer) -> Bool)
  