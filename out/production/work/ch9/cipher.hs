module Cipher where

import Data.Char

numAlphabet = 26 :: Int
ordA = ord 'A'
orda = ord 'a'

caesar :: Int -> String -> String
caesar _ "" = ""
caesar n (x:xs)
  | x `elem` ['A'..'Z'] =  caesar' ordA : caesar n xs
  | x `elem` ['a'..'z'] =  caesar' orda : caesar n xs
  | otherwise = x : caesar n xs
  where
    caesar' base = chr . (+base). flip mod numAlphabet . subtract base .(+n). ord $ x

unCaesar :: Int -> String -> String
unCaesar n = caesar $ negate n

caesar'' :: Int -> String -> String
caesar'' _ "" = ""
caesar'' n (x:xs) =
  let
  tr
    | x `elem` ['A'..'Z'] = caesar' ordA
    | x `elem` ['a'..'z'] = caesar' orda
    | otherwise = x
  in
    tr : caesar'' n xs
  where
    caesar' base = chr . (+base). flip mod numAlphabet . subtract base .(+n). ord $ x
