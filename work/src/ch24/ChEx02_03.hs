{-# LANGUAGE OverloadedStrings #-}
module ChEx02_03 where

import Control.Applicative ((<|>), (*>))
import Text.Trifecta
import Test.Hspec

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
  digits <- try (many parseDigit)
  if null digits
  then fail "not integer"
  else return $ foldl (\acc d -> read [d] + acc * 10) 0 digits

base10IntegerGood :: Parser Integer
base10IntegerGood = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = (char '-' *> (negate <$> base10IntegerGood)) <|> base10IntegerGood

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parseC :: String -> Maybe Char
parseC = maybeSuccess . parseString parseDigit mempty

parseI :: String -> Maybe Integer
parseI = maybeSuccess . parseString base10IntegerGood mempty

parseI' :: String -> Maybe Integer
parseI' = maybeSuccess . parseString base10Integer' mempty

main :: IO ()
main = hspec $ do

  describe "ex02: Parse digit" $ do
    it "can parse head digit char" $ do
      parseC "123"  `shouldBe` Just '1'
      parseC "098"  `shouldBe` Just '0'
    it "cannot parse head non-digit char" $
      parseC "abc"  `shouldBe` Nothing

  describe "ex02: Parse base10Integer" $ do
    it "can parse integer" $ do
      parseI "123"     `shouldBe` Just 123
      parseI "123abc"  `shouldBe` Just 123
    it "cannot parse non integer" $ do
      parseI "abc"     `shouldBe` Nothing
      parseI "abc123"  `shouldBe` Nothing

  describe "ex03: Parse base10Integer'" $ do
    it "can parse integer" $ do
      parseI' "123"     `shouldBe` Just 123
      parseI' "123abc"  `shouldBe` Just 123
    it "can parse integer with minus" $ do
      parseI' "-123"     `shouldBe` Just (-123)
      parseI' "-123abc"  `shouldBe` Just (-123)
    it "cannot parse non integer" $ do
      parseI' "abc"     `shouldBe` Nothing
      parseI' "abc123"  `shouldBe` Nothing
    it "cannot parse non integer with minus" $ do
      parseI' "abc"     `shouldBe` Nothing
      parseI' "abc123"  `shouldBe` Nothing
      parseI' "abc-"     `shouldBe` Nothing
      parseI' "abc-123"  `shouldBe` Nothing
