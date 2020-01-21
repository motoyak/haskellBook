module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation err a =
    Failure' err
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success' x) = Success' (f x)
  fmap _ (Failure' err) = Failure' err
  
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' err) (Success' _) = Failure' err
  (<*>) (Failure' err) (Failure' err') = Failure' (err `mappend` err')
  (<*>) (Success' f) (Success' x) = Success' (f x)
  (<*>) (Success' f) (Failure' err) = Failure' err


data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success :: Validation [Errors] Int
success =  Success' (+1) <*> Success' 1

res1 = success == Success' 2 -- should be true

failure :: Validation [Errors] Int
failure = Success' (+1) <*> Failure' [StackOverflow]

res2 = failure == Failure' [StackOverflow] -- should be true


failures:: Validation [Errors] Int
failures = Failure' [MooglesChewedWires] <*> Failure' [StackOverflow]

res3 = failures == Failure' [MooglesChewedWires, StackOverflow] -- should be true

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Monoid e, Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [ (1, return $ Failure' e)
              , (1, return $ Success' a)]
          
instance Arbitrary Errors where
  arbitrary = frequency [ (1, return DividedByZero)
                        , (1, return StackOverflow)
                        , (1, return MooglesChewedWires)]

type Target = (String, Bool, Int)
target :: Target
target = undefined


main :: IO ()
main = do
  quickBatch $ functor ((Success' target) :: Validation [Errors] Target)
  quickBatch $ applicative ((Success' target) :: Validation [Errors] Target)