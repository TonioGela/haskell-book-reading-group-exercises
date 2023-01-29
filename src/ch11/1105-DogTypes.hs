module Ch11.DogTypes where

data Doggies a = 
  Husky a |
  Mastiff a deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge
-- 1. Doggies is a type constructor
-- 2. :k Doggies :: * -> *
-- 3. :k Doggies String :: *
-- 4. :t Husky 10 :: Num a => Doggies a
-- 5. :t Husky (10 :: Integer) :: Doggies Integer
-- 6. :t Mastiff "Scooby Doo" :: Doggies String
-- 7. DogueDeBordeaux is both type constructor and data constructor
-- 8. :t DogueDeBordeaux :: a -> DogueDeBordeaux a
-- 9. :t DogueDeBordeaux "doggie!" :: DogueDeBordeaux String