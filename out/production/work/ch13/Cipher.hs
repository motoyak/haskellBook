module Cipher (caesar, vigenere) where

import Data.Char

numAlp :: Int
numAlp = 26
ordA :: Int
ordA = ord 'A'
orda :: Int
orda = ord 'a'

unCaesar :: Int -> String -> String
unCaesar n = caesar $ negate n

caesar :: Int -> String -> String
caesar _ "" = ""
caesar n (x:xs) =
  let
  tr
    | x `elem` ['A'..'Z'] = caesar' ordA
    | x `elem` ['a'..'z'] = caesar' orda
    | otherwise = x
  in
    tr : caesar n xs
  where
    caesar' base = chr . (+base). flip mod numAlp . subtract base .(+n). ord $ x


vigenere :: String -> String -> String
vigenere "" xs = xs
vigenere _ "" = ""
vigenere originalK originalX = vigenere' originalK originalX
  where
  vigenere' "" xs = vigenere' originalK xs
  vigenere' _ "" = ""
  vigenere' (k:ks) (x:xs)
    | (k `elem` ['A'..'Z'] || k `elem` ['a'..'z']) && (x `elem` ['A'..'Z'] || x `elem` ['a'..'z']) =
      let
        baseK = getOrd k
        baseX = getOrd x
        tr
          | x `elem` ['A'..'Z'] || x `elem` ['a'..'z'] = rShift (rShiftNum k) x
          | otherwise = x
        rShift n = chr . (+baseX) . flip mod numAlp . subtract baseX . (+n) . ord
        rShiftNum = subtract baseK . ord
        getOrd a
          | a `elem` ['A'..'Z'] = ordA
          | a `elem` ['a'..'z'] = orda
          | otherwise = 0
      in
        tr : vigenere' ks xs
    | otherwise = x : vigenere' (k:ks) xs


getCaesar :: Int -> IO String
getCaesar n = do
  word <- getLine
  return $ caesar n word

getVigenere :: String -> IO String
getVigenere key = do
  word <- getLine
  return $ vigenere key word
