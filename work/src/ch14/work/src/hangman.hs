{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck

zipper' :: Char -> Char -> Maybe Char -> Maybe Char
zipper' nowGuessed wordChar discoveredChar =
  if wordChar == nowGuessed
  then Just wordChar
  else discoveredChar

discoveredGen :: String -> Gen [Maybe Char]
discoveredGen word = do
  res <- mapM f word
  return res
  where
   f c = do
     a <- arbitrary :: Gen Bool
     if a
     then return (Just c)
     else return Nothing

testFillInCharacter :: IO ()
testFillInCharacter = quickCheck . verbose $
  property $
   \(word :: String)
    (guessed :: [Char])
    (c :: Char) ->
    forAll (discoveredGen word :: Gen [Maybe Char]) $ \discovered ->
      let
        newDiscovered =
          let zd = zipper' c
          in zipWith zd word discovered
        newGuessed = c : guessed
        Puzzle w d g = fillInCharacter (Puzzle word discovered guessed) c
      in w == word && d == newDiscovered && g == newGuessed

main :: IO ()
main = putStrLn "hello"

data Puzzle = Puzzle String [Maybe Char] [Char]

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      let zd = (zipper c)
      in zipWith zd word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick\
               \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the\
               \ word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed
