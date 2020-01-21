module Sum where

data Sum a b =
   First a
 | Second b
 deriving (Eq, Show)
 
instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _ = First x
  (<*>) _ (First y) = First y
  (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second x) k = k x