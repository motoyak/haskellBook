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
