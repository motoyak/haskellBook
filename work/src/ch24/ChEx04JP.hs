module ChEx04JP where

import Text.Trifecta
import Test.Hspec
import Control.Monad (replicateM)
import Control.Applicative ((<|>))

type AreaCode = Int
type SubscriberNumber1 = Int
type SubscriberNumber2 = Int

data PhoneNumber =
  PhoneNumber AreaCode
              SubscriberNumber1 SubscriberNumber2
  deriving (Eq, Show)
  
parsePhone :: Parser PhoneNumber
parsePhone = do
  (planArea, subscNum1, subscNum2) <- parseParts
  return $ PhoneNumber planArea subscNum1 subscNum2

seqDigits :: Int -> Parser String
seqDigits n = replicateM n digit

parseParts :: Parser (AreaCode, SubscriberNumber1, SubscriberNumber2)
parseParts = do
  areaCode <- parseAreaCode
  _ <- parseSeparator
  subscNum1 <- seqDigits 3
  _ <- parseSeparator
  subscNum2 <- seqDigits 4
  return (read areaCode :: Int, read subscNum1 :: Int, read subscNum2 :: Int)

parseAreaCode :: Parser String
parseAreaCode =
      try parenthesizedAreaCode
  <|> try prefixedAreaCode
  <|> try (seqDigits 3)

parenthesizedAreaCode :: Parser String
parenthesizedAreaCode = char '(' *> seqDigits 3 <* char ')'

prefixedAreaCode :: Parser String
prefixedAreaCode = sequence_ [digit, char '-'] >> seqDigits 3

parseSeparator :: Parser String
parseSeparator = many $ oneOf ['-', ' ']

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parsePhoneJP :: String -> Maybe PhoneNumber
parsePhoneJP = maybeSuccess . parseString parsePhone mempty

main :: IO ()
main = hspec $
  describe "US/Canada PhoneNumber" $ do
    it "can parse separeted with hyphone" $
      parsePhoneJP "023-456-7890" `shouldBe` Just (PhoneNumber 023 456 7890)
    it "can parse successive digits" $
      parsePhoneJP "1234567890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse parenthes" $
      parsePhoneJP "(123) 456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse prefixed" $
      parsePhoneJP "1-123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)