module Chapter25 () where

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose fga) = Compose $ (fmap . fmap) h fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose h) <*> (Compose fga) = Compose $ fmap (<*>) h <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap h (Compose fga) = (foldMap . foldMap) h fga
