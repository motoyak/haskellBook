module MyZip where

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip xs ys = go xs ys []
  where
    go _  [] acc = reverse acc
    go [] _  acc = reverse acc
    go (a:as) (b:bs) acc = go as bs $ (a, b) : acc

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f xs ys = go xs ys []
  where
    go _ []  acc = reverse acc
    go [] _  acc = reverse acc
    go (a:as) (b:bs) acc = go as bs $ f a b : acc

myZip' :: [a] -> [b] -> [(a,b)]
myZip' xs ys = myZipWith (,) xs ys


myZip'' :: [a] -> [b] -> [(a,b)]
myZip'' _ [] = []
myZip'' [] _ = []
myZip'' (x:xs) (y:ys) = (x,y) : myZip'' xs ys

myZipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith'' _ _ [] = []
myZipWith'' _ [] _ = []
myZipWith'' f (x:xs) (y:ys) = f x y : myZipWith'' f xs ys
