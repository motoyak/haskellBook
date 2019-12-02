{-# LANGUAGE FlexibleInstances #-}

module MayBeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) (First' Nada) (First' (Only a)) = First' (Only a)
  (<>) (First' (Only a)) (First' Nada) = First' (Only a)
  (<>) (First' (Only a)) (First' (Only b)) = First' (Only a)

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

instance Arbitrary (First' String) where
  arbitrary =
    frequency [ (1, return (First' Nada))
              , (1, x)]
    where
      x = do
        a <- (arbitrary :: Gen String)
        return $ First' (Only a)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
