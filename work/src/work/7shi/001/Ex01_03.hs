module Ex01_03 where

-- 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 2

fib' :: Integer -> Integer
fib' n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib' (n-1) + fib' (n-2)
  
-- 3
fib'' :: Integer -> Integer
fib'' n = case n of
  0 -> 0
  1 -> 1
  _
    | n > 0 -> fib'' (n-1) + fib'' (n-2)
    | otherwise -> 0