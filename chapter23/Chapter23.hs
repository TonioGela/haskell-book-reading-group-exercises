module Chapter23 () where 


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State $ \s -> let (a, s') = g s
                                              (f', s'') = f s'
                                          in (f' a, s'')

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s -> let (a, s') = f s
                                      (State h) = g a
                                  in h s'
-----------------------
---Chapter Exercises---
-----------------------


get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec sa = snd . runState sa

eval :: State s a -> s -> a
eval sa = fst . runState sa
