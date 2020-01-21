module Instances where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Either' a b
  = Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' a) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y
  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

data Tuple a b =
  Tuple a
        b
  deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple x y) = Tuple x (f y)

instance Monoid a => Applicative (Tuple a) where
  pure = Tuple mempty
  (Tuple u f) <*> (Tuple v x) = Tuple (u `mappend` v) (f x)

instance Foldable (Tuple a) where
  foldMap f (Tuple _ y) = f y
  foldr f z (Tuple _ y) = f y z

instance Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y

data Identity' a =
  Identity' a
  deriving (Eq, Ord, Show)
  
instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' (f x)

instance Foldable Identity' where
  foldMap f (Identity' x) = f x
  foldr f z (Identity' x) = f x z

instance Traversable Identity' where
  traverse f (Identity' x) = Identity' <$> f x

data Compose' f g a = Compose' (f (g a))

instance (Functor f, Functor g) => Functor (Compose' f g) where
  fmap f (Compose' x) = Compose' (fmap (fmap f) x)
  --fmap f (Compose' x) = Compose' ((f <$>) <$> x)
instance (Applicative f, Applicative g) => Applicative (Compose' f g) where
  pure x = Compose' (pure (pure x))
  (<*>) (Compose' f) (Compose' x) = Compose' ((<*>) <$> f <*>x)
  
instance (Foldable f, Foldable g) => Foldable (Compose' f g) where
  foldMap f (Compose' x) = foldMap (foldMap f) x
  
instance (Traversable f, Traversable g) => Traversable (Compose' f g) where
  traverse f (Compose' x) = Compose' <$> traverse (traverse f) x
  
type TriggerType = (Int, Int, [Int])

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => EqProp (Identity' a) where
  (=-=) = eq

testIdentity' = do
  quickBatch $ traversable (undefined :: Identity' TriggerType)