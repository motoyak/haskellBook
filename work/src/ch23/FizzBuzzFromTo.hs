module FizzBuzzFromTo where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo n m
  | n > m = fizzBuzzFromTo m n
  | n == m = fizzBuzzList [n]
  | otherwise = fizzBuzzList [m, m-1..n]

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5  == 0 = "Buzz"
  | n `mod` 3  == 0 = "Fizz"
  | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
