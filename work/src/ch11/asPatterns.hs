module AsPatterns where

import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xAll@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xAll ys

testIsSubseqOf :: IO ()
testIsSubseqOf = do
  print $ isSubseqOf "blah" "blahwoot"
  print $ isSubseqOf "blah" "wootblah"
  print $ isSubseqOf "blah" "wboloath"
  print $ isSubseqOf "blah" "wootbla"
  print $ isSubseqOf "blah" "halbwoot"
  print $ isSubseqOf "blah" "blawhoot"


capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords xs = map pairing $ words xs
  where
    pairing "" = ("", "")
    pairing xAll@(x:xs) = (xAll, toUpper x : xs)
