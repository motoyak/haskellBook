{-# LANGUAGE InstanceSigs #-}
module Moi where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}
  deriving (Show)

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s ->
      let (a0, s0) = g s
      in (f a0, s0)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) =
    Moi $ \s ->
      let (f0, s0) = f s
          (g0, s1) = g s0
      in (f0 g0, s1)

instance Monad (Moi s) where
  return = pure
  
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let (f0, s0) = f s
      in runMoi (g f0) s0
      
instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Moi s a) where
  arbitrary = Moi <$> arbitrary

instance (EqProp s, EqProp a) => EqProp (Moi s a) where
  (=-=) = (=-=)

type TriggerType = (Int, Int, [Int])

-- doesn't work
test = do
  verboseBatch $ functor (undefined :: Moi TriggerType TriggerType)