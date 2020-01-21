module InText where

listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr = foldr

mysum :: [Integer] -> Integer
mysum [] = 0
mysum (x:xs) = x + mysum xs

mylength :: [a] -> Integer
mylength [] = 1
mylength (_:xs) = 1 + mylength xs

myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl f q ls =
  q : (case ls of
         [] -> []
         x:xs -> myScanl f (f q x) xs)

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
fibs20 = take 20 fibs
fibsLT100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [2..]
