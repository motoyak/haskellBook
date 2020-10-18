{-# LANGUAGE InstanceSigs #-}
module ReaderT where

newtype ReaderT r m a =
  ReaderT {runReaderT :: r -> m a}
  
instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap.fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x  = ReaderT $ pure $ pure x
  f <*> a = ReaderT $ (<*>) <$> runReaderT f <*> runReaderT a
  
instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: Monad m
        => ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  ReaderT rma >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r