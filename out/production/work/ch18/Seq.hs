module Seq where

import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
  
sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"
  
sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"
  
binding :: IO ()
binding = do
  name <-getLine
  putStrLn name
  
binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y hello thar: " ++ name)
  
bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
    \name ->
      putStrLn ("y hello thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <-getLine
  
  putStrLn "age pls:"
  age <- getLine
  
  putStrLn ("y hello thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  
   \name ->
    putStrLn "age pls:" >>
    getLine >>=
    
     \age ->
      putStrLn ("y hello thar: "
                ++ name ++ " who is: "
                ++ age ++ " years old.")
  