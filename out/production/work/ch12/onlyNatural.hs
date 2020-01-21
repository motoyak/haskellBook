module OnlyNatural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ Zero) = 1
natToInteger (Succ (Succ x)) = 1 + natToInteger (Succ x)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0  = Nothing
  | otherwise = Just (go x)
    where
      go :: Integer -> Nat
      go y
        | y == 0 = Zero
        | otherwise = Succ (go (y-1))
