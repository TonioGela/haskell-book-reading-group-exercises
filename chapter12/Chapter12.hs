module Chapter12 () where


import Data.Char
import Data.List ((\\))

notThe :: String -> Maybe String
notThe st = if map toLower st == "the" then Nothing else Just st

nothingToa :: Maybe String -> String
nothingToa (Just a) = a
nothingToa Nothing = "a"

replaceThe :: String -> String
replaceThe st = unwords $ map (nothingToa . notThe) $ words st


vowels :: String
vowels = "aeiouy"

consonants :: [Char]
consonants = ['a'..'z'] \\ vowels

isVowel :: Char -> Bool
isVowel = (`elem` vowels) . toLower

isConsonant :: Char -> Bool
isConsonant =  (`elem` consonants) . toLower

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go ("the":x:xs) = if isVowel . head $ x then 1 + go xs else go (x:xs)
        go (_:xs) = go xs
        go _ = 0

countVowel :: String -> Int
countVowel = length . filter isVowel

countConsonant :: String -> Int
countConsonant = length . filter isConsonant

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord st = if (countConsonant st - countVowel st) < 0
              then Nothing else Just . Word' $ st


data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = integerToNat (n - 1) >>= Just . Succ


isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True


isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

maybeCons :: Maybe a ->  Maybe [a] -> Maybe [a]
maybeCons (Just a) (Just xs) = Just (a:xs)
maybeCons _ _ = Nothing

{-
maybeCons :: Maybe a ->  Maybe [a] -> Maybe [a]
maybeCons ma mxs = (:) <$> ma <*> mxs
--}

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr maybeCons (Just [])

either' ::  (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherLeftCons :: Either a b -> [a] -> [a]
eitherLeftCons (Left a) xs = a : xs
eitherLeftCons _ xs = xs

eitherRightCons :: Either a b -> [b] -> [b]
eitherRightCons (Right b) xs = b : xs
eitherRightCons _ xs = xs

lefts' :: [Either a b] -> [a]
lefts' = foldr eitherLeftCons []

rights' :: [Either a b] -> [b]
rights' = foldr eitherRightCons []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  (Just (a1,b1)) -> a1 : myUnfoldr f b1

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (f a, a))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing -> Leaf
               (Just (a1, b1, a2)) -> Node (unfold f a1) b1 (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\m -> if m >= n
                       then Nothing else Just (m + 1, m , m + 1)) 0
