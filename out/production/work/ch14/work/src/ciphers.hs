{-# LANGUAGE ScopedTypeVariables #-}
module Ciphers where

import Data.Char
import Test.QuickCheck

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


unVigenere :: String -> String -> String
unVigenere key = vigenere $ revKey key

revKey :: String -> String
revKey = map rev
  where
    rev c
      | c `elem` ['a'..'z'] = flip doRev c (ord 'a', ord 'z')
      | c `elem` ['A'..'Z'] = flip doRev c (ord 'A', ord 'Z')
      | otherwise = c
    doRev base = chr. flip subtract (snd base). flip mod 26 . subtract (fst base+1) . ord


testVigenere :: IO ()
testVigenere = quickCheck . verbose $
  property $
   \ (key  :: String)
     (text :: String) ->
    (unVigenere key . vigenere key) text == text

testCaesar :: IO ()
testCaesar = quickCheck . verbose $
  property $
   \ (key  :: Int)
     (text :: String) ->
    (unCaesar key . caesar key) text == text
