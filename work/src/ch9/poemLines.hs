module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines xs = go xs []
  where
    go str acc
      | str == "" = reverse acc
      | otherwise = go (dropWhile (=='\n') . dropWhile (/='\n') $ str) (takeWhile (/='\n') str : acc)

myLines'' :: String -> [String]
myLines'' "" = []
myLines'' xs = (takeWhile (/='\n') xs) : (myLines'' (dropWhile(=='\n') . dropWhile (/='\n') $ xs))

myWords :: String -> [String]
myWords xs = go xs []
  where
    go str acc
      | str == "" = reverse acc
      | otherwise = go (dropWhile (==' ') . dropWhile (/=' ') $ str) (takeWhile (/=' ') str : acc)

myWords'' :: String -> [String]
myWords'' "" = []
myWords'' xs = (takeWhile (/=' ') xs) : (myWords'' (dropWhile (=='\n'). dropWhile (/='\n') $xs))


shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

splitGreedy :: Char -> String -> [String]
splitGreedy delim xs = go xs []
  where
    go str acc
      | str == "" = reverse acc
      | otherwise = go (dropWhile (==delim) . dropWhile (/=delim) $ str) (takeWhile (/=delim) str : acc)

splitGreedy' :: Char -> String -> [String]
splitGreedy' _ "" = []
splitGreedy' delim xs = (takeWhile (/=delim) xs) : (splitGreedy' delim (dropWhile (==delim) . dropWhile (/=delim) $ xs))

myWords' = splitGreedy' ' '
myLines' = splitGreedy' '\n'

main :: IO ()
main =
  print $
    "Are they equal? "
    ++ show (myLines' sentences == shouldEqual)
