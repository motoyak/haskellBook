module Ex08_10 where

import Debug.Trace


sample :: [Integer]
sample = [4,3,5,7,1,10,9,8,6,2]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y = x:y:ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

isort' :: (Show a, Ord a) => [a] -> [a]
isort' [] = []
isort' (x:xs) = trace dbg1 $ trace dbg2 ret
  where
    ret = insert x xs'
    xs' = isort' xs
    dbg1 = "isort " ++ show (x:xs) ++ " = " ++
           "insert " ++ show x ++
           " (isort " ++ show xs ++ ")"
    dbg2 = "insert " ++ show x ++ " " ++ show xs' ++
           " = " ++ show ret
-- 8
bswap :: Ord a => [a] -> [a]
bswap [] = []
bswap [x] = [x]
bswap (x:y:ys)
  | x < y = y: bswap (x:ys)
  | otherwise = x: bswap (y:ys)

bswap' :: Ord a => [a] -> [a]
bswap' [] = []
bswap' [x] = [x]
bswap' (x:xs)
  | x > y = y:x:ys
  | otherwise = x:y:ys
  where
    (y:ys) = bswap' xs

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort xs = y : bsort ys
  where
    (y:ys) = reverse $ bswap xs

bsort' :: Ord a => [a] -> [a]
bsort' [] = []
bsort' xs = y : bsort' ys
  where
    (y:ys) = bswap' xs


-- 9
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take h xs)) (msort (drop h xs))
  where h = length xs `div` 2

--10
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort lt ++ [x] ++ qsort gt
  where
    lt = [y | y <- xs, y < x]
    gt = [y | y <- xs, y > x]
