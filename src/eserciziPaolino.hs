-- Tree.hs

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

data TreeL a = LeafL a | NodeL (TreeL a) (TreeL a)

instance Functor TreeL where 
    fmap :: (a -> b) -> TreeL a -> TreeL b
    fmap f (LeafL a) = LeafL (f a)
    fmap f (NodeL l r) = NodeL (fmap f l) (fmap f r) 


newtype Trie a = Trie [(a, Trie a)]

instance Functor Trie where 
    fmap f (Trie xs) = Trie $ fmap (f *** fmap f) xs

newtype F a = F (Int -> (a,a))


instance Functor F where
    fmap f (F h) = F $ \i -> let (a, a') = (h i) in (f a, f a')


(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')

leftMap:: (a -> c) -> Either a b -> Either c b
bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d

-- c.hs
newtype C a = C ((a -> Int) -> Int) -- si puo'

runC (C f) = f id

instance Functor C where 
    fmap :: (a -> b) -> C a -> C b
    fmap f (C g) = C $ \h -> g $ h . f

--- Dimostrazione che C è un funtore 

--- fmap id (C g) = C $ \h -> g $ (h . id) 
            ---   = C $ \h -> g h 
            ---   = C g 

--- fmap f . fmap l (C g) = fmap f $ (C \h -> g $ (h . l))
 ---                      = C \h' -> \h -> g $ (h . l) $ h' . f  (definizione di $)
 ---                      = C \h -> g (h . f . l) 
 ---                      = C \h -> g (h . (f . l))
 ---                      = fmap (f . l) (C g)


-- main.hs

main = putStrLn "Hello, world!"

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Functor laws
-- fmap id == id
-- fmap (f . g) == fmap f . fmap g

(<$>) :: Functor f => (a -> b)
($) :: (a -> b) -> f a -> f b -> a -> b


n = Nothing
w = Just "woohoo"
ave = Just "Ave"

lms :: []
lms = [ave, n, w]

replaceWithP :: Char -> b -> Char 
replaceWithP = const 'p'

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y

-- replaceWithP lms
-- (fmap . fmap) replaceWithP lms

data Wrap (f :: * -> *) a = Wrap (f a) deriving (Eq, Show)

-- Wrap = * -> *
-- (f a) = * -> *
-- se a = * allora f = * -> *

instance Functor f => Functor (Wrap f) where
    fmap g (Wrap fa) = Wrap (fmap g fa)


instance Functor f => Functor (Wrap f) where
  fmap g (Wrap fa) = Wrap (fmap g fa)

data Wrap2 (f :: * -> *) a = Wrap2 (f a) a deriving (Eq, Show)

instance Functor f => Functor (Wrap2 f) where 
    fmap :: (a -> b) -> Wrap2 f a -> Wrap2 f b
    fmap g (Wrap2 fa a) = Wrap2 (fmap g fa) (g a) -- edoardo ? -- non compila con a /= b

data Wrap3 (f :: * -> *) h a = Wrap3 (f a) (h a) deriving (Eq, Show)

instance Functor (f,h) => Functor (Wrap3 f h) where 
    fmap :: (a -> b) -> Wrap3 f h a -> Wrap3 f h b
    fmap g (Wrap3 fa ha) = Wrap3 (fmap g fa) (fmap g ha) -- ok

instance Functor (f,h,k) => Functor (Wrap4 f h k) where
    fmap :: (a -> b) -> Wrap4 f h k -> Wrap4 f h k b
    fmap g (Wrap4 fa ha ka) = Wrap4 (fmap g fa) (fmap g ha) (fmap g ka) -- paolo, ci sta? si per sempre

newtype C a = C ((a -> Int) -> Int) -- si puo'

runC (C f) = f

instance Functor C where 
    fmap :: (a -> b) -> C a -> C b
    fmap f (C g) = C $ \h -> g $ h . f
         
         --- g :: (a -> Int) -> Int 
         --- h :: b -> Int
         ---- f :: a -> b 
         ---- h . f :: a -> Int  
         
--- fmap id (C g) = C $ \h -> g $ (h . id) 
            ---   = C $ \h -> g h 
            ----  = C g 

--- fmap f . fmap l (C g) = fmap f $ (C \h -> g $ (h . l))
                          = C \h' -> \h -> g $ (h . l) $ h' . f  (definizione di $)
                          = C \h -> g (h . f . l) 
                          = C \h -> g (h . (f . l))
                          = fmap (f . l) (C g)
    

getInt :: IO Int
getInt = fmap read getLine
