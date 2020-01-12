{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Dice where

import Control.Monad

a = [1,2,3,4,5,6]
b = [7,8,9,10,11,12]

p=1/6

ab = a <> b

allCases'''' = [[x,y] | x <- ab, y<-ab, x < y]<>[[x,y,z] | x <- ab, y<-ab,z<-ab, x < y && y < z]
  <> [[x,y,z,i] | x <- ab, y<-ab,z<-ab,i<-ab, x < y && y < z && z<i]
  <> [[x,y,z,i,j] | x <- ab, y<-ab,z<-ab,i<-ab, j<-ab, x < y && y < z && z<i && i < j]
  <> [[x,y,z,i,j,k] | x <- ab, y<-ab,z<-ab,i<-ab, j<-ab, k<-ab, x < y && y < z && z<i && i < j && j < k]

allCases' :: [Int] -> Int -> [[Int]]
allCases' xs 1 = [[x] | x<-xs]
allCases' xs n = allCases' xs (n-1) ++ [x ++ [y] | x <- allCases' xs $ (n-1),  y <- xs , maximum x < y]

--allCases :: [Int] -> Int -> [[Int]]
--allCases xs 1 =  [[x] | x<-xs]
--allCases xs n =

allCases :: [Int] -> Int -> [[Int]]
allCases xs n = do
  num <- forM [1..n] $ \i -> do
    x <- xs
    return x
  return num
  
  
makeCases :: [Int] -> Int -> [[Int]]
makeCases xs 1 = [[x]|x<-xs]
makeCases xs n =
  filter (\x->length x > 0) $ concatMap (\x-> fmap (\y-> if maximum x > maximum y then y++x else []) (makeCases xs (n-1))) xss
    where xss = [[x]| x<-xs]

d = length $ makeCases [1..12] 10