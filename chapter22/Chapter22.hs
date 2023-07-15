{-# OPTIONS_GHC -Wno-type-defaults #-}
module Chapter22 () where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader h) = Reader $ f . h

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (Reader rf) <*> (Reader f) = Reader $ \r -> rf r (f r)

instance Monad (Reader r) where
  return = pure
  (Reader h) >>= f = Reader $ \r -> runReader (f (h r)) r

---Exercise: Ask

asks :: (r -> a) -> Reader r a
asks = Reader

data Person = Person { name :: String, age :: Int } deriving (Eq, Show)

data Dog = Dog { dogsName :: String, dogsAge :: Int } deriving (Eq, Show)

getDog :: Reader Person Dog
getDog = Dog <$> asks name <*> asks age

-----------------------
---Chapter Exercises---
-----------------------

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'


summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just (3 :: Integer), Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7
  print $ and $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
