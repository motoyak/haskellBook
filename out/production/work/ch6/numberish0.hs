module Numberish where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberigh :: Numberish a => a -> a -> a
sumNumberigh a a' = fromNumber summed
  where integerOfA  = toNumber a
        integerOfA' = toNumber a'
        summed = integerOfA + integerOfA'
