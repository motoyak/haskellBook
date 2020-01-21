module ExLibraryFunctions where

import Data.Monoid

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y-> Any (x == y))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr (\y z -> if z then True else y==x) False

-- 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
  foldr
  (\x y ->
    case y of
    Nothing -> Just x
    Just z -> if x < z then Just x else Just z)
  Nothing

-- 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
  foldr
  (\x y ->
    case y of
    Nothing -> Just x
    Just z -> if x > z then Just x else Just z)
  Nothing

-- 6
null' :: (Foldable t) => t a -> Bool
null' = not.getAny.foldMap (const $ Any True)

null'' :: (Foldable t) => t a -> Bool
--null'' = not.foldr (\_ _ -> True) False
null'' = foldr (\_ _ -> False) True

-- 7
length' :: (Foldable t) => t a -> Int
length' xs =
  if null xs
  then 0
  else getSum.foldMap (const $ Sum 1) $ xs

length'' :: (Foldable t) => t a -> Int
length'' = foldr (\_ acc -> acc+1) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x->[x])

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldr (\x y-> [x] ++ y) []

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--foldMap' f = foldr (\x y -> (f x) `mappend` y) mempty
foldMap' f = foldr (f.mappend) mempty

-- appendix
foldr' :: (Foldable t) => (a -> b -> b) -> b -> t a ->b
foldr' f z xs = foldMap