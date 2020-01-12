module ExCombinations where

import Control.Applicative (liftA3)
import Data.List (sort)

stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"

combThree' :: String -> String -> [(Char, Char, Char)]
combThree' stops vowels = [(s1, v, s2) | s1<-stops, s2<-stops, v<-vowels]


combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos x y z = liftA3 (,,) x y z

test :: Bool
test = (sort . combThree' stops $ vowels) == (sort . combos stops vowels $ stops)