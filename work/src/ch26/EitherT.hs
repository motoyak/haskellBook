module EitherT where

newtype EitherT e m a =
  EitherT {runEitherT :: m (Either e a)}

-- 1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap.fmap) f ema
-- 2
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  f <*> a = EitherT $ (<*>) <$> runEitherT f  <*> runEitherT a
-- 3
instance Monad m => Monad (EitherT e m) where
  return = pure
  v >>= f =
    EitherT $ do
      x <- runEitherT v
      case x of
        Left e -> return $ Left e
        Right a -> runEitherT $ f a
        
-- 4
swapEither :: Either e a
           -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ fmap swapEither ema

-- 5
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) = do
  v <- amb
  case v of
    Left a -> f a
    Right b -> g b
 