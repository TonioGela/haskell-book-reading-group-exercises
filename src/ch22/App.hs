module Ch22.App where

import Control.Applicative (liftA2)

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

--

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address
} deriving (Eq, Show)

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- with Reader and lift
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- (>>=) :: m a     -> (a -> m b)     -> m b
-- (>>=) :: r -> a  -> (a -> r -> b)  -> r -> b
getDogRM' :: Person -> Dog
getDogRM' = dogName >>= (\dn p -> Dog dn (address p))
