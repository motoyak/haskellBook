module Constant where

newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
