module WarmUpAndReview where

stops = "pbtdkg" :: String
vowels = "aeiou" :: String

combThree :: String -> String -> [(Char, Char, Char)]
combThree stops vowels = [(s1, v, s2) | s1<-stops, s2<-stops, v<-vowels]

combThree' :: String -> String -> [(Char, Char, Char)]
combThree' stops vowels = [(s1, v, s2) | s1<-stops, s1=='p', s2<-stops, v<-vowels]

nowns = ["book", "water", "tree", "box"]
verbs = ["tell", "speak", "see", "visit"]

combThree'' :: [String] -> [String] -> [(String, String, String)]
combThree'' nowns verbs = [(n1, v, n2) | n1 <-nowns, n2<-nowns, v<-verbs]

seekritFunc' :: String -> Double
seekritFunc' x =
  (fromIntegral . sum . map length $ words x) /
  (fromIntegral . length . words $ x)
