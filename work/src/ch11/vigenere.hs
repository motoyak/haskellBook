module Vigenere where

import Data.Char

numAlp = 26 :: Int
ordA = ord 'A'
orda = ord 'a'


vigenere :: String -> String -> String
vigenere "" xs = xs
vigenere _ "" = ""
vigenere originalK originalX = vigenere' originalK originalX
  where
  vigenere' "" xs = vigenere' originalK xs
  vigenere' _ "" = ""
  vigenere' (k:ks) (x:xs)
    | x `elem` ['A'..'Z'] || x `elem` ['a'..'z'] =
      let
        baseX = getOrd x
        tr
          | x `elem` ['A'..'Z'] || x `elem` ['a'..'z'] = rShift (rShiftNum k) x
          | otherwise = x
        rShift n = chr . (+baseX) . flip mod numAlp . subtract baseX . (+n) . ord
        rShiftNum = subtract baseX . ord
        getOrd a
          | a `elem` ['A'..'Z'] = ordA
          | a `elem` ['a'..'z'] = orda
          | otherwise = 0
      in
        tr : vigenere' ks xs
    | otherwise = x : vigenere' (k:ks) xs
