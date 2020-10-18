module ChEx04 where

import Text.Trifecta
import Test.Hspec
import Control.Monad (replicateM)
import Control.Applicative ((<|>))

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)
  
parsePhone :: Parser PhoneNumber
parsePhone = do
  (planArea, exchange, lineNumber) <- parseParts
  return $ PhoneNumber planArea exchange lineNumber

seqDigits :: Int -> Parser String
seqDigits n = replicateM n digit

parseParts :: Parser (NumberingPlanArea, Exchange, LineNumber)
parseParts = do
  planArea <- parsePlanArea
  _ <- parseSeparator
  exchange <- seqDigits 3
  _ <- parseSeparator
  lineNumber <- seqDigits 4
  return (read planArea :: Int, read exchange :: Int, read lineNumber :: Int)

parsePlanArea :: Parser String
parsePlanArea =
      try parenthesizedPlanArea
  <|> try prefixedPlanArea
  <|> try (seqDigits 3)

parenthesizedPlanArea :: Parser String
parenthesizedPlanArea = char '(' *> seqDigits 3 <* char ')'

prefixedPlanArea :: Parser String
prefixedPlanArea = sequence_ [digit, char '-'] >> seqDigits 3

parseSeparator :: Parser String
parseSeparator = many $ oneOf ['-', ' ']

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parsePhoneUS :: String -> Maybe PhoneNumber
parsePhoneUS = maybeSuccess . parseString parsePhone mempty

main :: IO ()
main = hspec $
  describe "US/Canada PhoneNumber" $ do
    it "can parse separeted with hyphone" $
      parsePhoneUS "123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse successive digits" $
      parsePhoneUS "1234567890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse parenthes" $
      parsePhoneUS "(123) 456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse prefixed" $
      parsePhoneUS "1-123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)