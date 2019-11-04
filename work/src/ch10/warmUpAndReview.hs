module WarmUpAndReview where

stops = "pbtdkg" :: String
vowels = "aeiou" :: String

combThree :: String -> String -> [(Char, Char, Char)]
combThree stops vowels = [(s1, v, s2) | s1<-stops, s2<-stops, v<-vowels]

combThree' :: String -> String -> [(Char, Char, Char)]
combThree' stops vowels = [(s1, v, s2) | s1<-stops, s1=='p', s2<-stops, v<-vowels]
