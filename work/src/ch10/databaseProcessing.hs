module DatabaseProcessing where

-- | :set -package time
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 8001
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f a b = case a of
                  DbDate time -> time : b
                  _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f a b = case a of
                  DbNumber num -> num : b
                  _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb item = sum / num
  where
    sum = fromIntegral . sumDb $ item
    num = fromIntegral . length . filterDbNumber $ item
