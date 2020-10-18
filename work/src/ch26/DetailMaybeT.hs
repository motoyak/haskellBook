module DetailMaybeT where

newtype Identity a =
  Identity {runIdentity :: a}

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity fab) <*> (Identity a) = Identity $ fab a


innerMost :: [Maybe (Identity (a -> b))]
          -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap.fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)]
        -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)]
       -> [Maybe (Identity a)]
       -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a -> b))]
         -> [Maybe (Identity a)]
         -> [Maybe (Identity b)]
lmiApply f = final' (second' (innerMost f))

