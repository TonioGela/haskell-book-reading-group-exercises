{-# LANGUAGE GADTs #-}

module Chapter26 () where

-- | MaybeT
data MaybeT m a where
  MaybeT :: {runMaybeT :: m (Maybe a)} -> MaybeT m a

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure . pure) x
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

-- | EitherT
data EitherT e m a where
  EitherT :: {runEitherT :: m (Either e a)} -> EitherT e m a

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) =
    EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  (EitherT fab) <*> (EitherT mea) =
    EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left e -> return $ Left e
        Right y -> runEitherT (f y)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where
    swapEither (Left e) = Right e
    swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g = (either f g =<<) . runEitherT
