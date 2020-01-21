module Divide where

  type Numerator = Integer
  type Denominator = Integer
  type Quotient = Integer

  dividedBy' :: Numerator -> Denominator -> Quotient
  dividedBy' = div

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where
      go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

  data DividedResult = Result Integer | DividedByZero deriving(Show)

  dividedBySafe :: Integral a => a -> a -> (DividedResult, a)
  dividedBySafe num denom = go num denom 0
    where
      go n d count
        | d == 0 = (DividedByZero, 0)
        | n >= 0 && d > 0 = goPP n d count
        | n <  0 && d > 0 = goNP n d count
        | n >= 0 && d < 0 = goPN n d count
        | otherwise       = goNN n d count

      goPP n d count
        | n < d = (Result count, n)
        | otherwise = goPP (n - d) d (count + 1)

      goNP n d count
        | n >= 0 = (Result $ -count, n)
        | otherwise = goNP (n + d) d (count + 1)

      goPN n d count
        | n <= 0 = (Result $ -count, n)
        | otherwise = goPN (n + d) d (count + 1)

      goNN n d count
        | n > d = (Result count, n)
        | otherwise = goNN (n - d) d (count + 1)
