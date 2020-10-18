{-# LANGUAGE InstanceSigs #-}

module Compose where
import Control.Applicative (liftA2)

newtype Identity a =
  Identity {runIdentity :: a}

newtype Compose f g a =
  Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap.fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h)
    => Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap.fmap.fmap) f fgha

v :: Compose []
             Maybe
             (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]


instance (Applicative f, Applicative g)
    => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure (pure x)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
--  (Compose f) <*> (Compose a) = Compose (liftA2 (<*>) f a)
  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose x)= foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose x) = Compose <$> traverse (traverse f) x
  
instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (>>=) = undefined
