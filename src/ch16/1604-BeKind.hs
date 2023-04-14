-- Given a type signature, determine the kinds of each type variable

-- a -> a
-- :k a :: *

-- a -> b a -> T (b a)
-- :k b :: * -> *
-- :k T :: * -> *

-- c a b -> c b a
-- :k c :: * -> * -> *
