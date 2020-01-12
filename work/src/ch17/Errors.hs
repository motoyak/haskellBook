module Errors where

import Data.Either.Validation


data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success :: Validation [Errors] Int
success =  Success (+1) <*> Success 1

res1 = success == Success 2

failure :: Validation [Errors] Int
failure = Success (+1) <*> Failure [StackOverflow]

res2 = failure == Failure [StackOverflow]


failures:: Validation [Errors] Int
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]

res3 = failures == Failure [MooglesChewedWires, StackOverflow]