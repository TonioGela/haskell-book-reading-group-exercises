module Ch22.CE where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
--x1 = (,) <$> xs <*> ys
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
--x2 = (,) <$> ys <*> zs
x2 = liftA2 (,) ys zs

x3 i = (,) (z' i) (z' i)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just a) = a
fromMaybe' d _ = d

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

f1 x = foldr (&&) True $ sequA x
f2 = sequA $ fromMaybe' 0 s'
f3 = bolt <$> ys
f3' = bolt $ fromMaybe' 0 ys
