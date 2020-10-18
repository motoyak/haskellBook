{-# LANGUAGE OverloadedStrings #-}
module ChEx01 where

import Control.Applicative ((<|>))
import Text.Trifecta
import Test.Hspec

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSS y) = compare x y

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 rel1 _) (SemVer major2 minor2 patch2 rel2 _)
    | major1 /= major2 = compare major1 major2
    | minor1 /= minor2 = compare minor1 minor2
    | patch1 /= patch2 = compare patch1 patch2
    | null rel1 && null rel2 = EQ
    | null rel1 = GT
    | null rel2 = LT
    | otherwise = compare rel1 rel2


parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseCore
  rel <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch rel metadata

skipComma :: Parser Char
skipComma = char '.'

parseCore :: Parser (Major, Minor, Patch)
parseCore = do
  major <- integer
  _ <- skipComma
  minor <- integer
  _ <- skipComma
  patch <- integer
  return (major, minor, patch)

parseRelease :: Parser Release
parseRelease = char '-' *> parseNoses

parseMetadata :: Parser Metadata
parseMetadata = char '+' *> parseNoses

alpha :: Parser Char
alpha = oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

alphaNumHyphen :: Parser Char
alphaNumHyphen = alpha <|> digit <|> char '-'

parseNos :: Parser NumberOrString
parseNos = do
  v <-    (NOSI <$> try (decimal <* notFollowedBy alphaNumHyphen))
      <|> (NOSS <$> some alphaNumHyphen)
  _ <- skipMany (oneOf ".")
  return v

parseNoses :: Parser [NumberOrString]
parseNoses = some parseNos

skipSepRelease :: Parser Char
skipSepRelease = char '-'

skipSepMetadata :: Parser Char
skipSepMetadata = char '+'

-- test cases are referred from https://github.com/scarvalhojr/haskellbook/blob/master/chapter24/section24.11-1.hs 

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parse :: String -> Maybe SemVer
parse = maybeSuccess . parseString parseSemVer mempty

main :: IO ()
main = hspec $ do

  describe "Parse semantic version" $ do
    it "without labels" $ do
      parse "1.2.3"  `shouldBe` Just (SemVer 1 2 3 [] [])
      parse "0.0.0"  `shouldBe` Just (SemVer 0 0 0 [] [])
      parse "-1.1.1" `shouldBe` Just (SemVer (-1) 1 1 [] [])
      parse "1.-1.1" `shouldBe` Just (SemVer 1 (-1) 1 [] [])
      parse "1.1.-1" `shouldBe` Just (SemVer 1 1 (-1) [] [])
      parse "01.1.1" `shouldBe` Just (SemVer 1 1 1 [] [])
      parse "1.01.1" `shouldBe` Just (SemVer 1 1 1 [] [])
      parse "1.1.01" `shouldBe` Just (SemVer 1 1 1 [] [])
      parse ".2.3"   `shouldBe` Nothing
      parse "1..3"   `shouldBe` Nothing
      parse "1.2."   `shouldBe` Nothing
      parse "1.."    `shouldBe` Nothing
      parse ".2."    `shouldBe` Nothing
      parse "..3"    `shouldBe` Nothing
      parse ".."     `shouldBe` Nothing
      parse "1.2"    `shouldBe` Nothing
      parse "1."     `shouldBe` Nothing
      parse "1"      `shouldBe` Nothing
      parse "a"      `shouldBe` Nothing
      parse "-"      `shouldBe` Nothing
      parse " "      `shouldBe` Nothing
      parse ""       `shouldBe` Nothing
      parse "a.0.0"  `shouldBe` Nothing
      parse "0.a.0"  `shouldBe` Nothing
      parse "0.0.a"  `shouldBe` Nothing

    it "with release labels only" $ do
      parse "1.2.3-a"  `shouldBe`
        Just (SemVer 1 2 3 [NOSS "a"] [])
      parse "1.2.3-1"  `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1] [])
      parse "1.2.3-a1"  `shouldBe`
        Just (SemVer 1 2 3 [NOSS "a1"] [])
      parse "1.2.3-alpha-1.9.-x-y-z.1"  `shouldBe`
        Just (SemVer 1 2 3 [NOSS "alpha-1",
                                 NOSI 9,
                                 NOSS "-x-y-z",
                                 NOSI 1] [])
      parse "1.2.3-a-"  `shouldBe`
        Just (SemVer 1 2 3 [NOSS "a-"] [])
      parse "1.2.3-a."   `shouldBe`
        Just (SemVer 1 2 3 [NOSS "a"] [])
      parse "1.2.3-"     `shouldBe` Nothing
      parse "1.2.3-."    `shouldBe` Nothing
      parse "1.2.3-1."   `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1] [])
      parse "1.2.3-1.1." `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1, NOSI 1] [])
      parse "1.2.3-1:"   `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1] [])
      parse "1.2.3-1?"   `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1] [])
      parse "1.2.3-1a"   `shouldBe`
        Just (SemVer 1 2 3 [NOSS "1a"] [])
      parse "1.2.3-0"  `shouldBe`
        Just (SemVer 1 2 3 [NOSI 0] [])
      parse "1.2.3-01" `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1] [])

    it "with metadata labels only" $ do
      parse "1.2.3+a"  `shouldBe`
        Just (SemVer 1 2 3 [] [NOSS "a"])
      parse "1.2.3+1"  `shouldBe`
        Just (SemVer 1 2 3 [] [NOSI 1])
      parse "1.2.3+a1"  `shouldBe`
        Just (SemVer 1 2 3 [] [NOSS "a1"])
      parse "1.2.3+alpha-1.9.-xyz.1"  `shouldBe`
        Just (SemVer 1 2 3 [] [NOSS "alpha-1",
                                    NOSI 9,
                                    NOSS "-xyz",
                                    NOSI 1])
      parse "1.2.3+"     `shouldBe` Nothing
      parse "1.2.3+."    `shouldBe` Nothing
      parse "1.2.3+a."   `shouldBe`
        Just (SemVer 1 2 3 [] [NOSS "a"])
      parse "1.2.3+1."   `shouldBe`
        Just (SemVer 1 2 3 [] [NOSI 1])
      parse "1.2.3+1.1." `shouldBe`
        Just (SemVer 1 2 3 [] [NOSI 1, NOSI 1])
      parse "1.2.3+1:"   `shouldBe`
        Just (SemVer 1 2 3 [] [NOSI 1])
      parse "1.2.3+1+"   `shouldBe`
        Just (SemVer 1 2 3 [] [NOSI 1])
      parse "1.2.3+1a"   `shouldBe`
        Just (SemVer 1 2 3 [] [NOSS "1a"])

    it "with release and metadata labels" $ do
      parse "1.2.3-a+b" `shouldBe`
        Just (SemVer 1 2 3 [NOSS "a"] [NOSS "b"])
      parse "1.2.3-1.alpha-2.9+2.beta-1.99" `shouldBe`
        Just (SemVer 1 2 3 [NOSI 1,
                                 NOSS "alpha-2",
                                 NOSI 9]
                                [NOSI 2,
                                 NOSS "beta-1",
                                 NOSI 99])
      parse "1.2.3-a+"  `shouldBe` Nothing
      parse "1.2.3-.+a" `shouldBe` Nothing
      parse "1.2.3-+a"  `shouldBe` Nothing
      parse "1.2.3-+a." `shouldBe` Nothing

  describe "Compare version labels separately" $ do

    it "single indetifiers" $ do
      []                    < [NOSI 0]     `shouldBe` True
      []                    < [NOSS "a"] `shouldBe` True
      [NOSI 9]     < [NOSS "a"] `shouldBe` True
      [NOSI 0]     < [NOSI 1]     `shouldBe` True
      [NOSS "a"] < [NOSS "z"] `shouldBe` True
      [NOSS "z"] > [NOSI 0]     `shouldBe` True

    it "multiple indetifiers" $ do
      compare [NOSI 0, NOSI 9]
              [NOSI 0, NOSS "a"]
              `shouldBe` LT
      compare [NOSI 0, NOSI 9]
              [NOSI 0, NOSI 9, NOSS "a"]
              `shouldBe` LT
      compare [NOSI 0, NOSI 9, NOSS "a"]
              [NOSI 0, NOSI 9, NOSS "a"]
              `shouldBe` EQ
      compare [NOSI 0, NOSI 9, NOSS "b"]
              [NOSI 0, NOSI 9, NOSS "a"]
              `shouldBe` GT

  describe "Compare parsed semantic versions" $ do

    it "without version labels" $ do
      parse "1.9.0"  `compare` parse "1.10.0" `shouldBe` LT
      parse "1.10.0" `compare` parse "1.11.0" `shouldBe` LT
      parse "1.10.9" `compare` parse "1.11.0" `shouldBe` LT
      parse "1.10.0" `compare` parse "1.10.1" `shouldBe` LT
      parse "1.10.9" `compare` parse "2.0.0"  `shouldBe` LT
      parse "1.2.3"  `compare` parse "1.2.3"  `shouldBe` EQ

    it "with release labels only" $ do
      parse "1.2.3-0"     `compare` parse "1.2.3"       `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3"       `shouldBe` LT
      parse "1.2.3-0"     `compare` parse "1.2.3-1"     `shouldBe` LT
      parse "1.2.3-0"     `compare` parse "1.2.3-a"     `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3-b"     `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3-a0"    `shouldBe` LT
      parse "1.2.3-a.0"   `compare` parse "1.2.3-a.1"   `shouldBe` LT
      parse "1.2.3-a.0"   `compare` parse "1.2.3-a.0.0" `shouldBe` LT
      parse "1.0.0-alpha" `compare` parse "1.0.0"       `shouldBe` LT
      parse "1.2.3-a.0.0" `compare` parse "1.2.3-a.0.0" `shouldBe` EQ

    it "with metadata labels only" $ do
      parse "1.2.3"       `compare` parse "1.2.3+0"     `shouldBe` EQ
      parse "1.2.3"       `compare` parse "1.2.3+a"     `shouldBe` EQ
      parse "1.2.3+0"     `compare` parse "1.2.3+1"     `shouldBe` EQ
      parse "1.2.3+0"     `compare` parse "1.2.3+a"     `shouldBe` EQ
      parse "1.2.3+a"     `compare` parse "1.2.3+b"     `shouldBe` EQ
      parse "1.2.3+a"     `compare` parse "1.2.3+a0"    `shouldBe` EQ
      parse "1.2.3+a.0"   `compare` parse "1.2.3+a.1"   `shouldBe` EQ
      parse "1.2.3+a.0"   `compare` parse "1.2.3+a.0.0" `shouldBe` EQ
      parse "1.2.3+a.0.0" `compare` parse "1.2.3+a.0.0" `shouldBe` EQ

    it "with release and metadata labels" $ do
      parse "1.2.3-0"       `compare` parse "1.2.3-0+0"     `shouldBe` EQ
      parse "1.2.3-0"       `compare` parse "1.2.3-0+a"     `shouldBe` EQ
      parse "1.2.3-0+0"     `compare` parse "1.2.3-0+1"     `shouldBe` EQ
      parse "1.2.3-0+0"     `compare` parse "1.2.3-0+a"     `shouldBe` EQ
      parse "1.2.3-0+a"     `compare` parse "1.2.3-0+b"     `shouldBe` EQ
      parse "1.2.3-0+a"     `compare` parse "1.2.3-0+a0"    `shouldBe` EQ
      parse "1.2.3-0+a.0"   `compare` parse "1.2.3-0+a.1"   `shouldBe` EQ
      parse "1.2.3-0+a.0"   `compare` parse "1.2.3-0+a.0.0" `shouldBe` EQ
      parse "1.2.3-0+a.0.0" `compare` parse "1.2.3-0+a.0.0" `shouldBe` EQ

    it "matches examples in the definition page" $ do
      parse "1.0.0-alpha"      `compare` parse "1.0.0-alpha.1"    `shouldBe` LT
      parse "1.0.0-alpha.1"    `compare` parse "1.0.0-alpha.beta" `shouldBe` LT
      parse "1.0.0-alpha.beta" `compare` parse "1.0.0-beta"       `shouldBe` LT
      parse "1.0.0-beta"       `compare` parse "1.0.0-beta.2"     `shouldBe` LT
      parse "1.0.0-beta.2"     `compare` parse "1.0.0-beta.11"    `shouldBe` LT
      parse "1.0.0-beta.11"    `compare` parse "1.0.0-rc.1"       `shouldBe` LT
      parse "1.0.0-rc.1"       `compare` parse "1.0.0"            `shouldBe` LT