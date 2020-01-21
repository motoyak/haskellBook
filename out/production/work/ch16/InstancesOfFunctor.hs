{-# LANGUAGE ScopedTypeVariables #-}

module InstancesOfFunctor where

import Test.QuickCheck

functorIdentity ::
  (Functor f, Eq (f a)) =>
     f a
  -> Bool
functorIdentity f = fmap id f == f

functorCompose ::
  (Functor f, Eq (f c)) =>
     (a -> b)
  -> (b -> c)
  -> f a
  -> Bool
functorCompose f g x =
  fmap g (fmap f x) == fmap (g . f) x


newtype Identity a = Identity a deriving (Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

testIdentity = do
  quickCheck . property $
    \ (x :: Int) ->
    functorIdentity (Identity x)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Int) ->
    functorCompose f g (Identity x)


data Pair a = Pair a a deriving (Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

testPair = do
  quickCheck . property $
    \ (x :: Int)
      (y :: Int) ->
    functorIdentity (Pair x y)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Int)
      (y :: Int) ->
    functorCompose f g (Pair x y)


data Two a b = Two a b deriving (Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

testTwo = do
  quickCheck . property $
    \ (x :: String)
      (y :: Int) ->
    functorIdentity (Two x y)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: String)
      (y :: Int) ->
    functorCompose f g (Two x y)


data Three a b c = Three a b c deriving (Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

testThree = do
  quickCheck . property $
    \ (x :: String)
      (y :: Maybe Char)
      (z :: Int) ->
    functorIdentity (Three x y z)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: String)
      (y :: Maybe Char)
      (z :: Int) ->
    functorCompose f g (Three x y z)


data Three' a b = Three' a b b deriving (Eq)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

testThree' = do
  quickCheck . property $
    \ (x :: String)
      (y :: Int)
      (z :: Int) ->
    functorIdentity (Three' x y z)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: String)
      (y :: Int)
      (z :: Int) ->
    functorCompose f g (Three' x y z)


data Four a b c d = Four a b c d deriving (Eq)

instance Functor (Four a b c) where
  fmap f (Four x y z t) = Four x y z (f t)

testFour = do
  quickCheck . property $
    \ (x :: String)
      (y :: Maybe String)
      (z :: Char)
      (t :: Int)
      ->
    functorIdentity (Four x y z t)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: String)
      (y :: Maybe String)
      (z :: Char)
      (t :: Int)
      ->
    functorCompose f g (Four x y z t)


data Four' a b = Four' a a a b deriving (Eq)

instance Functor (Four' a) where
  fmap f (Four' x y z t) = Four' x y z (f t)

testFour' = do
  quickCheck . property $
    \ (x :: String)
      (y :: String)
      (z :: String)
      (t :: Int)
      ->
    functorIdentity (Four' x y z t)
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: String)
      (y :: String)
      (z :: String)
      (t :: Int)
      ->
    functorCompose f g (Four' x y z t)
