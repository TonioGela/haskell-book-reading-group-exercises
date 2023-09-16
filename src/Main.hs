module Main (main) where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "hello"
  putStrLn "world"

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join (fmap f ma)

lM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lM2 f ma mb = f <$> ma <*> mb

binding :: IO ()
binding = getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = do
  putStrLn "name pls:"
  (getLine >>= \n -> putStrLn $ "" ++ n)

twiceWhenEven :: [Char] -> [Char]
twiceWhenEven xs = do
  x <- xs
  if 's' == x
    then [x, x]
    else []

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
  noEmpty name' >>=
    \nammy -> noNegative age' >>=
      \agey -> noNegative weight' >>=
        \weighty -> weightCheck (Cow nammy agey weighty)

-- f :: Maybe Integer
-- f = Just 1
-- g :: Maybe Integer
-- g = Just 2
-- h :: Maybe Integer
-- h = Just 3
-- doSomething :: Maybe (Integer, Integer, Integer)
-- doSomething = do
--   a <- f
--   b <- g
--   c <- h
--   pure (a, b, c)
-- doSomethingA = f *> g *> h *> f





-- f' :: Integer -> Maybe Integer
-- f' 0 = Nothing
-- f' n = Just n

-- g' :: Integer -> Maybe Integer
-- g' i =
--   if even i
--     then Just (i + 1)
--     else Nothing

-- h' :: Integer -> Maybe String
-- h' i = Just ("10191" ++ show i)

-- doSomething' :: Integer -> Maybe (Integer, Integer, String)
-- doSomething' n = do
--   a <- f' n
--   b <- g' a
--   c <- h' b
--   pure (a, b, c)

-- doSomethingA' = f' *> g'

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure = Second
  (First e) <*> _ = First e
  (Second f) <*> a = fmap f a

instance Monad (Sum a) where
  (Second a) >>= f = f a
  (First e) >>= _ = First e

