module GivenATypeWriteTheFunction where
  i :: a -> a
  i x = x
-- i = (\x -> x)

  c :: a -> b -> a
  c x y = x
-- c = (\x y -> x)

-- c and c'' are identical
  c'' :: b -> a -> b
-- c'' = (\y x -> y)
  c'' y x = y

  c' :: a -> b -> b
-- c' = (\x y -> y)
  c' x y = y

-- r has many possibe definitions whose numbers are dependent on length [a]
  r :: [a] -> [a]
  -- r = (\x -> [head x])
  r x = [head x]

  r1 :: [a] -> [a]
-- r1 = (\x -> tail x)
  r1 x = tail x

  r2 :: [a] -> [a]
--  r2 = (\x -> take 3 x)
  r2 x = take 3 x
-- and so on

  co :: (b -> c) -> (a -> b) -> a -> c
-- co = (\x1 x2 x3 -> co' x1 x2 x3)
-- where co' bToC aToB xa = (bToC (aToB xa))
  co bToC aToB xa = bToC $ aToB xa

  a :: (a -> c) -> a -> a
-- a = (\x1 x2 -> a' x1 x2)
-- where a' _ xa = xa
  a _ xa = xa

  a' :: (a -> b) -> a -> b
-- a' = (\x1 x2 -> a'' x1 x2)
-- where a'' aToB xa = aToB xa
  a' aToB xa = aToB xa
