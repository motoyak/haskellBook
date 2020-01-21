module Kleisli where

import Control.Monad ((>=>))

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

sayHi' :: Int -> IO String
sayHi' number = do
  putStrLn $ show number
  getLine


readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you?"
  
getAge' :: Int -> IO Int
getAge' = sayHi' >=> readM
