module Ex04_05 where

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

-- 4
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n _ | n < 1 = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' n xs | n < 1 = xs
drop' n (_:xs) = drop' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = go xs []
  where
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)
    
-- 5
fact :: Integer -> Integer
fact n = product [1..n]