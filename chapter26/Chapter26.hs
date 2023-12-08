{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chapter26 () where

import Control.Monad ((<=<))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Reader
  ( ReaderT (ReaderT),
    ask,
    runReader,
    runReaderT,
  )
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Bifunctor (first)
import Data.Functor

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
eitherT f g = either f g <=< runEitherT

-- | ReaderT
data ReaderT' r m a where
  ReaderT' :: {runReaderT' :: r -> m a} -> ReaderT' r m a

instance Functor m => Functor (ReaderT' r m) where
  fmap f (ReaderT' rma) =
    ReaderT' $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT' r m) where
  pure x = ReaderT' $ (pure . pure) x
  (ReaderT' rmab) <*> (ReaderT' rma) =
    ReaderT' $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT' r m) where
  return = pure
  (ReaderT' rma) >>= f =
    ReaderT' $ \r -> do
      a <- rma r
      runReaderT' (f a) r

-- | StateT
data StateT' s m a where
  StateT' :: {runStateT' :: s -> m (a, s)} -> StateT' s m a

instance Functor m => Functor (StateT' s m) where
  fmap f (StateT' sma) =
    StateT' $ (fmap . fmap) (first f) sma

instance Monad m => Applicative (StateT' s m) where
  pure x = StateT' $ \s -> pure (x, s)
  (StateT' smab) <*> (StateT' sma) =
    StateT' $ \s -> do
      (ab, s') <- smab s
      (a, s'') <- sma s'
      return (ab a, s'')

instance Monad m => Monad (StateT' s m) where
  return = pure
  (StateT' sma) >>= f =
    StateT' $ \s -> do
      (a, s') <- sma s
      runStateT' (f a) s'

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans MaybeT where
  lift = MaybeT . fmap pure

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap pure

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (StateT' s) where
  lift :: Monad m => m a -> StateT' s m a
  lift ma = StateT' $ \s -> fmap (,s) ma

-- | Chapter exercises
rDec :: Num a => ReaderT a Identity a
rDec = do
  num <- ask
  return (num - 1)

rDec' :: Num a => ReaderT a Identity a
rDec' = ask <&> subtract 1

testRDec :: Bool
testRDec = runReader rDec (1 :: Integer) == 0

rShow :: Show a => ReaderT a Identity String
rShow = do
  a <- ask
  return . show $ a

rShow' :: Show a => ReaderT a Identity String
rShow' = ask <&> show

testRShow :: Bool
testRShow = runReader rShow (1 :: Integer) == "1"

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  a <- ask
  liftIO $ putStrLn $ "Hi: " ++ show a
  return (a + 1)

testRPrintAndInc :: IO ()
testRPrintAndInc = do
  a <- runReaderT rPrintAndInc (1 :: Integer)
  print a

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  a <- get
  liftIO $ putStrLn $ "Hi: " ++ show a
  put (a + 1)
  return (show a)

testSPrintIncAccum :: IO ()
testSPrintIncAccum = do
  a <- runStateT sPrintIncAccum (1 :: Integer)
  print a
