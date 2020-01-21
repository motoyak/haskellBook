module BuildingFunctions where

funcA :: String -> String
funcA xs = xs ++ "!"
funcB :: String -> Char
funcB xs = xs !! 4
funcC :: String -> String
funcC xs = drop 9 xs

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs xs = (drop 9 xs) ++ (take 4 (drop 5 xs)) ++ (take 5 xs)
