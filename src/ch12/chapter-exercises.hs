module Ch12.ChapterExercises where

-- 1.
-- What is the kind of `a`?
id :: a -> a
id = undefined
-- Answer: 
-- * :: *

-- 2.
-- What are the kinds of `a` and `f`?
r :: a -> f a
r = undefined
-- Answer:
-- a :: *
-- f :: * -> *

-- String processing
-- 1
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

swapTheWithA :: Maybe String -> String
swapTheWithA Nothing = "a"
swapTheWithA (Just s) = s

replaceThe :: String -> String
replaceThe = unwords . map (swapTheWithA . notThe) . words

-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countThe . map notThe . words

-- isSubsequenceOf is maybe better
isVowel c = if c == 'a' || c == 'A' || c == 'e' || c == 'E' || c == 'i' || c == 'I' || c == 'o' || c == 'O' || c == 'u' || c == 'U'
  then True
  else False
isFirstLetterVowel = isVowel . head
countThe [] = 0
countThe (Nothing:(Just a):as)
  | isFirstLetterVowel a = 1 + countThe as
  | otherwise = countThe as
countThe (a:as) = countThe as

-- 3.
countVowels :: String -> Integer
countVowels [] = 0
countVowels (a:as)
  | isVowel a = 1 + countVowels as
  | otherwise = countVowels as

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if partialSum > 0 then Just (Word' s) else Nothing
  where partialSum = foldl (\x c -> case isVowel c of
                        True -> x - 1
                        False -> x + 1
                      ) 0 s

mkWord' :: String -> Maybe Word'
mkWord' s = (\r -> if r > 0 then Just (Word' s) else Nothing) . sum $ (map sumVowelsOrCons s)
sumVowelsOrCons c = case isVowel c of
  True -> -1
  False -> 1


-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + (natToInteger a)


integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (integerToNat' i)

integerToNat' 0 = Zero
integerToNat' i = Succ (integerToNat' (i - 1))

-- Small library for Maybe
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _         = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ _ = b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe b _ = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just a) -> a) . filter (isJust)

-- TODO
-- flipMaybe :: [Maybe a] -> Maybe [a]
-- flipMaybe [] = Nothing
-- flipMaybe (Nothing:_) = Nothing
-- flipMaybe ((Just a):as) = Just (a:flipMaybe as)

-- Small library for Either
lefts' :: [Either a b] -> [a]
lefts' = foldr fn []
  where
    fn (Left a) acc = a:acc
    fn _ acc        = acc

rights' :: [Either a b] -> [b]
rights' = foldr fn []
  where
    fn (Right a) acc = a:acc
    fn _         acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr fn ([], [])
  where
    fn (Right a) (f, s) = (f, a:s)
    fn (Left a) (f, s)  = (a:f, s)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right a) = Just (f a)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

-- TODO
-- eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
-- eitherMaybe'' bToC (Left a) = Nothing
-- eitherMaybe'' bToC (Right b) = Just (bToC b)

-- TODO Unfolds