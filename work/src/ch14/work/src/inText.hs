module InText where

import Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1,2,2,2,2,2,2,2,2,2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeTuple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeTuple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additioanGreater :: Int -> Bool
prop_additioanGreater x = x + 1 > x

prop_additioanGreater' :: Int -> Bool
prop_additioanGreater' x = x + 0 > x || x == 0

runQc :: IO ()
runQc = quickCheck prop_additioanGreater'
