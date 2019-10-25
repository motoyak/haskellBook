module Recursion where
  q2 :: (Eq a, Num a) => a -> a
  q2 = go 0
    where
      go sum i
        | i == 0 = sum
        | otherwise = go (sum + i) (i - 1)

  q3 :: (Integral a) => a -> a -> a
  q3 x = go 0
    where
      go sum i
        | i == 0 = sum
        | otherwise = go (sum + x) (i - 1)
