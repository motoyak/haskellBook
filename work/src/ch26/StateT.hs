{-# LANGUAGE InstanceSigs #-}
module StateT where

newtype StateT s m a =
  StateT {runStateT :: s -> m (a,s)}
  
instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) =
    StateT $ \s -> (\(a,s') -> (f a, s')) <$> sma s
      
instance Monad m => Applicative (StateT s m) where
  pure a  = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  (StateT fmsab) <*> (StateT gmsa) =
    StateT $ \s -> do
      (f0, s0) <- fmsab s
      (g0, s1) <- gmsa s0
      return (f0 g0, s1)

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  StateT sma >>= f =
    StateT $ \s -> do
      (a,s') <- sma s
      runStateT (f a) s'