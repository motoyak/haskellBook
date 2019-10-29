module DataChar where

import Data.Char

onlyUpper :: String -> String
onlyUpper = filter isUpper

capitalizeHead :: String -> String
capitalizeHead "" = ""
capitalizeHead (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (x:[]) = [toUpper x]
capitalizeAll (x:xs) = (toUpper x) : capitalizeAll xs

capHeadOnly :: String -> Char
capHeadOnly = toUpper . head
