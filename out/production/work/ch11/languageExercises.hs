module LanguageExercises where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (x:xs)
  | x == ' ' || x == '.' = x : capitalizeWord xs
  | otherwise = toUpper x : xs

splitSentence' :: String -> [String]
splitSentence' "" = []
splitSentence' str = go str [] ""
  where
    go "" acc _ = acc
    go (x:xs) acc cur
      | x == '.' = go xs (acc ++ [cur++[x]]) ""
      | otherwise = go xs acc (cur ++ [x])

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord $ splitSentence xs

splitSentence :: String -> [String]
splitSentence s = case dropWhile (=='.') s of
                    "" -> []
                    s' -> w : splitSentence s''
                      where
                        (w, s'') = addC '.' . break (=='.') $ s'
                        addC :: Char -> (String, a) -> (String, a)
                        addC c (x, y) = (x ++ [c], y)
