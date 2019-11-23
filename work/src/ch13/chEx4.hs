module ChEx4 where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "name? : "
  name <- getLine
  putStr "age? : "
  age <- getLine
  case mkPerson name (read age :: Age) of
    Right person@(Person _ _) -> putStrLn $ "Yay! Successfully got a person: " ++ show person
    Left err -> print err
