module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = normalize line1
      normalize = filter (flip elem ['a'..'z']) . map toLower
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
