module GrabBag where

mThA x y z = x * y * z
mThB x y = \z -> x * y * z
mThC x = \y -> \z -> x * y * z
mThD = (\x -> \y -> \z -> x * y * z) :: Num a => a -> a -> a -> a

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addOneIfOdd'' = \n -> case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x

mflip' f x y = f y x
