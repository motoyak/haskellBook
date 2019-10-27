module MyWords where

myWords :: String -> [String]
myWords xs = go xs []
  where
    go str acc
      | str == "" = reverse acc
      | otherwise = go (dropWhile (==' ') . dropWhile (/=' ') $ str) (takeWhile (/=' ') str : acc)
