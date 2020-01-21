module ValidateTheWord where

import Data.Char

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
consonants = filter (not . flip elem vowels) ['a'..'z']

mkWord :: String -> Maybe Word'
mkWord xs =
  let
    numVowels = countVowels xs
    numConsonants = countConsonants xs
  in
    if numVowels > numConsonants
    then Nothing
    else Just (Word' xs)

isVowel :: Char -> Bool
isVowel = flip elem vowels . toLower

isConsonant :: Char -> Bool
isConsonant = flip elem consonants . toLower

countVowels :: String -> Integer
countVowels = foldr (\a b -> if isVowel a then (b+1) else b) 0

countConsonants :: String -> Integer
countConsonants = foldr (\a b -> if isConsonant a then (b+1) else b) 0
