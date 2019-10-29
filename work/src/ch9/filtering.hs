module Filtering where

-- on REPL
-- :l filtering.hs poemLines.hs

import PoemLines

myFilter :: String -> [String]
myFilter = filter (\x-> not $ elem x ["the", "a", "an"]) . myWords'

myFilter' :: String -> [String]
myFilter' = filter (not . flip elem ["the", "a", "an"]) . myWords'
