module Ch17.ListApplicative where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil Nil = Nil
  (<>) Nil (Cons a al)  = Cons a al
  (<>) (Cons a al) Nil = Cons a al
  (<>) (Cons a al) cc@(Cons b bl) = Cons (a) (al <> cc)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a    = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) at@(Cons a as) = (Cons (f a) (f <$> as)) <> (fs <*> at)
  
  
f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
r' = f <*> v
r'' = Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
r = r' == r''

