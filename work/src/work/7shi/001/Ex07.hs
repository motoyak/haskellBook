module Ex07 where

import Data.Char

rot13c :: Char -> Char
rot13c c
  | 'a' <= c && c <= 'z' = chr $ (ord c + 13 - ord 'a') `mod` 26 + ord 'a'
  | 'A' <= c && c <= 'Z' = chr $ (ord c + 13 - ord 'A') `mod` 26 + ord 'A'
  | otherwise = c
  
rot13 :: String -> String
rot13 = fmap rot13c