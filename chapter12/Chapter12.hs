module Chapter12 () where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x > 0 = Succ <$> integerToNat (x - 1)
  | otherwise = Nothing

-- Small library for Maybe

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f m = fromMaybe b $ f <$> m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = go []
  where
    go acc (Just x : xs) = go (acc ++ [x]) xs
    go acc (Nothing : xs) = go acc xs
    go acc [] = acc

-- omg I wrote it using foldr
catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr f []
  where
    f (Just x) acc = x : acc
    f Nothing acc = acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f (Just x) (Just xs) = Just (x : xs)
    f _ _ = Nothing

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) acc = a : acc
    f _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right b) acc = b : acc
    f _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f (Left a) (as, bs) = (a : as, bs)
    f (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)