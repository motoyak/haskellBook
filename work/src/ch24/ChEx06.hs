{-# LANGUAGE QuasiQuotes #-}

module ChEx05 where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import Data.List (intercalate, sort, nub)
import Text.Trifecta
import Text.RawString.QQ

data Log = Log [DayLog]

instance Show Log where
  show (Log dayLogs) = concatMap show dayLogs

data DayLog = DayLog
  { dlDate :: Date
  , dlActionInfo :: ActionInfo
  }

instance Show DayLog where
  show (DayLog date actionInfo) = unlines ["# " ++ show date, show actionInfo]

data Date = Date
  { year :: Int
  , month :: Int
  , day :: Int
  }

instance Show Date where
  show (Date y m d) = intercalate "-" [show y, padShow m, padShow d]

data ActionInfo = ActionInfo [ActionInfoItem] (Map.Map Activity [DiffTime])

type Activity = String

instance Show ActionInfo where
  show (ActionInfo items _) = unlines . map show $ items

data ActionInfoItem = ActionInfoItem
  { getTime :: Time
  , getActivity :: Activity
  }

instance Show ActionInfoItem where
  show (ActionInfoItem t a) = show t ++ " " ++ show a

makeActionInfo :: [ActionInfoItem] -> ActionInfo
makeActionInfo items = ActionInfo items infoMap
  where
    infoMap = Map.fromListWith (++) $ zip (map getActivity items) diffTimes
    diffTimes = map ((:[]) . uncurry makeDiffTime) $ zip times (tail times)
    times = map getTime items
    
idActionTime :: ActionInfo -> [(Activity, Int)]
idActionTime (ActionInfo _ mAD) =
  concatMap (\x -> map (\y -> (fst x, y )) (snd x)) (Map.toList $ ( Map.map (map getDiffTime) mAD))
  
sumUpActionTime :: ActionInfo -> [(Activity, Int)]
sumUpActionTime (ActionInfo _ mAD) =
  Map.toList . Map.map (sum . map getDiffTime) $ mAD
  
averageActionTime :: ActionInfo -> [(Activity, Double)]
averageActionTime (ActionInfo _ mAD) =
  Map.toList . Map.map (average . map getDiffTime) $ mAD
    where
      average xs = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

showEachLog :: Log -> [(Activity, Int)]
showEachLog (Log dayLogs) = sort $ concatMap (idActionTime.dlActionInfo) dayLogs

sumEachLog :: Log -> [(Activity, Int)]
sumEachLog (Log dayLogs) = sort $ concatMap (sumUpActionTime.dlActionInfo) dayLogs

sumUpLog :: Log -> [(Activity, Int)]
sumUpLog (Log dayLogs) = sort $ sumUp $ concatMap (sumUpActionTime.dlActionInfo) dayLogs
  where
    sumUp :: [(Activity, Int)] -> [(Activity, Int)]
    sumUp = foldl (\acc x -> case search (fst x) acc of
                               Nothing -> x : acc
                               Just y -> (fst x, y + snd x) : removeItem (fst x) acc) []

averageLogDBG :: Log -> [(Activity, Double, Int, Int, [Int])]
averageLogDBG (Log dayLogs) = sort . nub . averageAll $ concatMap (idActionTime.dlActionInfo) dayLogs
  where
    averageAll :: [(Activity, Int)] -> [(Activity, Double, Int, Int, [Int])]
    averageAll z = foldl (\acc x -> case filter (\y -> fst y == fst x) z of
                                    [] -> acc
                                    zs -> ( fst x
                                          , realToFrac(sum . fmap snd $ zs) / realToFrac(length zs)
                                          , length zs
                                          , sum . fmap snd $ zs
                                          , fmap snd $ zs
                                          ) : acc) [] z

averageLog :: Log -> [(Activity, Double)]
averageLog (Log dayLogs) = sort . nub . averageAll $ concatMap (idActionTime.dlActionInfo) dayLogs
  where
    averageAll :: [(Activity, Int)] -> [(Activity, Double)]
    averageAll z = foldl (\acc x -> case filter (\y -> fst y == fst x) z of
                                    [] -> acc
                                    zs -> ( fst x
                                          , realToFrac(sum . fmap snd $ zs) / realToFrac(length zs)
                                          ) : acc) [] z

search :: (Eq a) => a -> [(a,b)] -> Maybe b
search _ [] = Nothing
search x ((a,b):ys)
  | x == a  = Just b
  | otherwise = search x ys

removeItem :: (Eq a) => a -> [(a,b)] -> [(a,b)]
removeItem _ [] = []
removeItem x ((a,b):xs)
  | x == a = removeItem x xs
  | otherwise = (a,b) : removeItem x xs

newtype DiffTime = DiffTime
  { getDiffTime :: Int
  } deriving (Eq, Show)

data Time = Time
  { hour :: Int
  , minute :: Int
  }
  
instance Show Time where
  show (Time h m) = padShow h ++ ":" ++ padShow m

makeDiffTime :: Time -> Time -> DiffTime
makeDiffTime t1 t2 = DiffTime $ (toMinutes t2) - (toMinutes t1)
  where toMinutes (Time h m) = h * 60 + m

padShow :: Int -> String
padShow a
  | a < 10 = '0' : show a
  | otherwise = show a


--- Parse part ----

commentMark :: String
commentMark = "--"

dateMark :: String
dateMark = "#"

digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . map (toInteger . digitToInt)
  where
    listToInteger :: [Integer] -> Integer
    listToInteger = foldl1 (\acc a -> acc * 10 + a)

parseDate :: Parser Date
parseDate = do
  yearString <- count 4 digit
  _ <- char '-'
  monthString <- count 2 digit
  _ <- char '-'
  dayString <- count 2 digit
  return $
    Date
      (fromInteger . digitsToInteger $ yearString)
      (fromInteger . digitsToInteger $ monthString)
      (fromInteger . digitsToInteger $ dayString)

parseTime :: Parser Time
parseTime = do
  hourString <- count 2 digit
  _ <- char ':'
  minuteString <- count 2 digit
  return $
    Time
      (fromInteger . digitsToInteger $ hourString)
      (fromInteger . digitsToInteger $ minuteString)

comment :: (Monad m, CharParsing m) => m ()
comment = do
  skipOptional space
  _ <- string commentMark
  skipMany (notChar '\n') <?> "comment"

eol :: Parser ()
eol = do
  _ <- oneOf "\r\n"
  return () <?> "end of line"

commentSpace :: (Monad m, TokenParsing m) => m ()
commentSpace = do
  _ <- spaces
  _ <- skipMany $ token comment <?> "comments and whitespace"
  return ()

tokenC :: (Monad m, TokenParsing m) => m a -> m a
tokenC p = p <* (commentSpace <|> pure ())

parseActionInfoItem :: Parser ActionInfoItem
parseActionInfoItem = do
  time <- tokenC parseTime
  activity <- manyTill anyChar (try eol <|> try comment <|> eof)
  return $ ActionInfoItem time activity

parseActionInfo :: Parser ActionInfo
parseActionInfo = do
  items <- many $ tokenC parseActionInfoItem <?> "list of action info items"
  return $ makeActionInfo items

parseDayLog :: Parser DayLog
parseDayLog = do
  skipOptional commentSpace
  date <- symbol dateMark *> tokenC parseDate
  actionInfo <- parseActionInfo
  return $ DayLog date actionInfo
  
parseLog :: Parser Log
parseLog = do
  skipOptional commentSpace <?> "parselog skip"
  dayLogs <- some (tokenC parseDayLog) <?> "all day logs"
  return $ Log dayLogs

testLog :: String
testLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]