module LibEither where

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) =
  case x of
    Left y -> y : lefts' xs
    Right _ -> lefts' xs

rights' :: [Either a b] -> [b]
rights' [] = []
rights' (x:xs) =
  case x of
    Left _ -> rights' xs
    Right y -> y : rights' xs

rights'' :: [Either a b] -> [b]
rights'' = foldr
  (\a b ->
    case a of
      Left _ -> b
      Right x -> x:b ) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\y -> Just (f y))
