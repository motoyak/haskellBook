{-# LANGUAGE OverloadedStrings #-}

module Text.ExFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"
decimalExample = "509"

type DecimalOrFraction =
  Either Integer Rational

parseIof :: Parser DecimalOrFraction
parseIof = do
  try (Left <$> parseDecimal) <|> (Right <$> parseFraction)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = do
  v <- decimal
  eof
  return v

main :: IO ()
main = do
  let parseIof' =
        parseString parseIof mempty

  print $ parseIof' shouldWork
  print $ parseIof' shouldAlsoWork
  print $ parseIof' decimalExample


  print $ parseIof' alsoBad

  print $ parseIof' "3/"

  print $ parseIof' badFraction

  print $ parseIof' shouldWork