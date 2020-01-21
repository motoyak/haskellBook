module Phone where

import Data.List
import Data.Char
import Data.Maybe

-- validButtons = "1234567890*#"
type Digit = Char
-- valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [(Digit, String)] deriving (Show)

daPhone = DaPhone [
    ('1', "1")
  , ('2', "abc2")
  , ('3', "def3")
  , ('4', "ghi4")
  , ('5', "jkl5")
  , ('6', "mno6")
  , ('7', "pqrs7")
  , ('8', "tuv8")
  , ('9', "wxyz9")
  , ('0', " +_0")
  , ('*', "^*")
  , ('#', ".,#")
  ]

convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"
  ]

caps :: (Digit, Presses)
caps = ('*', 1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone c
  | isUpper c = [caps, findTap daPhone $ toLower c]
  | otherwise = [findTap daPhone c]

findTap :: DaPhone -> Char -> (Digit, Presses)
findTap (DaPhone []) _ = error "not found"
findTap (DaPhone (x:xs)) c
  | c `elem` snd x = (fst x, 1 + fromMaybe 0 (elemIndex c (snd x)))
  | otherwise = findTap (DaPhone xs) c

cellPhoneDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhoneDead daPhone = concatMap (reverseTaps daPhone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> snd a + b) 0

type Count = Int

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maxCounter . foldr (\a b -> if a==' ' then b else countUp a b) []

countUp :: Eq a => a -> [(a, Count)] -> [(a, Count)]
-- countUp ' ' xs = xs
countUp a xs =
  case find (\x -> a == fst x) xs of
    Nothing -> (a, 1) : xs
    Just x -> (a, 1 + snd x) : filter (\x -> a /= fst x) xs

maxCounter :: [(a, Count)] -> (a, Count)
maxCounter = foldr (\a b -> if snd a >= snd b then a else b) (undefined, 0)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

mostPopularWord :: [String] -> String
mostPopularWord = fst . maxCounter . foldr countUp []

coolestWord :: [String] -> String
coolestWord = mostPopularWord . concatMap words
