module Numberish where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberigh :: Numberish a => a -> a -> a
sumNumberigh a a' = fromNumber summed
  where integerOfA  = toNumber a
        integerOfA' = toNumber a'
        summed = integerOfA + integerOfA'
