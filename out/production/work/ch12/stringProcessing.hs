module StringProcessing where

import Data.Char

notThe :: String -> Maybe String
notThe xs
  | xs == "the" = Nothing
  | otherwise = Just xs

replaceThe :: String -> String
replaceThe "" = ""
replaceThe s = case break isSpace s of
                 (x0, "") -> x0
                 ("the", x:xs) -> "a " ++ replaceThe xs
                 (x0, x:xs) -> x0 ++ " " ++ replaceThe xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go s 0
  where
    go str acc =
      case break isSpace str of
        (_, "") -> acc
        ("the", x0:xs) ->
          case dropWhile (==' ') xs of
            "" -> acc
            y:ys ->
              if y `elem` "aeiou"
              then go (y:ys) (acc+1)
              else go (y:ys) acc
        (_, x:xs) -> go xs acc

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

onlyVowels :: String -> String
onlyVowels "" = ""
onlyVowels (x:xs) =
  if isVowel x
  then x : onlyVowels xs
  else onlyVowels xs

countVowels :: String -> Integer
countVowels "" = 0
countVowels str = count 0 . onlyVowels $ str
  where
    count acc "" = acc
    count acc (x:xs) = count (acc+1) xs

countVowels' :: String -> Integer
countVowels' = foldr (\a b -> if isVowel a then b+1 else b) 0
