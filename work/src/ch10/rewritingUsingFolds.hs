module RewritingFunctionsUsingFolds where

myAnd1 :: [Bool] -> Bool
myAnd1 [] = True
myAnd1 (x:xs) =
  if x == False
  then False
  else myAnd1 xs

myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd2 xs

myAnd3 :: [Bool] -> Bool
myAnd3 = foldr
  (\a b ->
    if a == False
    then False
    else b) True

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) =
  if x == True
  then True
  else myOr1 xs

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs

myOr3 :: [Bool] -> Bool
myOr3 = foldr
  (\a b ->
    if a == True
    then True
    else b) False

myAny :: (a->Bool) -> [a] -> Bool
myAny f = foldr (\a b-> f a || b) False

myAny1 :: (a->Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 f (x:xs) =
  if f x == True
  then True
  else myAny1 f xs

myAny2 :: (a->Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = f x || myAny1 f xs

myAny3 :: (a->Bool) -> [a] -> Bool
myAny3 f = foldr
  (\a b ->
    if f a == True
    then True
    else b) False


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a==x || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = any (==x) ys

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b-> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> (f a) ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f xs = head $ foldr (\a b -> if f a (head b) == GT then a:b else b) [last xs] xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f xs = head $ foldr (\a b -> if f a (head b) == LT then a:b else b) [last xs] xs
