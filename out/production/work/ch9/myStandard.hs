module MyStandard where

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) =
  if x == False
  then False
  else myAnd' xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' x xs = any (==x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys)
      | f y acc == GT = go y ys
      | otherwise = go acc ys

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys)
      | f y acc == LT = go y ys
      | otherwise = go acc ys

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
  where
    compare x y
      | x > y = GT
      | x == y = EQ
      | x < y = LT

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
  where
    compare x y
      | x > y = GT
      | x == y = EQ
      | x < y = LT
