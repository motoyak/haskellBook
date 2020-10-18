{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]


parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)


parseNos' :: Parser NumberOrString
parseNos' = do
  skipMany (oneOf "\n")
  v <-     (Left <$> integer)
       <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main = do
  let p f l = parseString f mempty l

  print $ p parseNos eitherOr

  print $ p (some letter) a
  print $ p integer b

  print $ p parseNos a
  print $ p parseNos b

  print $ p (many parseNos) c
  print $ p (some parseNos) c
