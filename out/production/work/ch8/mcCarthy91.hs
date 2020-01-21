module McCarthy91 where

  mc91 :: Integral a => a -> a
  mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11

-- map mc91 [95..110]
-- = [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
