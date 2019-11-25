{-# LANGUAGE ScopedTypeVariables #-}

module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity  = (*2) . half

fracGen :: (Arbitrary a, Fractional a, Eq a, Show a) => Gen a
fracGen = do
  a <- arbitrary
  return a

prop_half :: Property
prop_half =
  forAll (fracGen :: Gen Double)
  (\x -> halfIdentity x == x)

testHalf :: IO ()
testHalf = quickCheck prop_half


listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

orderedListGen :: (Arbitrary a, Ord a) => Gen [a]
orderedListGen = do
  a <- arbitrary
  return $ sort a

testListOrdered :: IO ()
testListOrdered = quickCheck $
  forAll (orderedListGen :: Gen [Integer]) listOrdered
  -- (\x -> listOrdered x)

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

plusGen :: (Arbitrary a, Num a, Eq a, Show a) => Gen a
plusGen = do
  a <- arbitrary
  return a

testPlusAssociative :: IO ()
testPlusAssociative = quickCheck . verbose $
  forAll (plusGen :: Gen Integer) $ \x ->
  forAll (plusGen :: Gen Integer) $ \y ->
  forAll (plusGen :: Gen Integer) $ \z ->
  plusAssociative x y z
  -- (\x y z-> plusAssociative x y z)

testPlusCommutative :: IO ()
testPlusCommutative = quickCheck . verbose $
  forAll (plusGen :: Gen Integer) plusCommutative
  -- (\x y -> plusCommutative x y)

testPlusCommutative' :: IO ()
testPlusCommutative' = quickCheck . verbose $
  property $ \x ->
  property $ \y ->
  plusCommutative (x::Integer) y

testPlusCommutative'' :: IO ()
testPlusCommutative'' = quickCheck . verbose $
  property $ \x y->
  plusCommutative (x::Integer) y


mulAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
mulAssociative x y z =
  x * (y * z) == (x * y) * z

mulCommutative :: (Num a, Eq a) => a -> a -> Bool
mulCommutative x y =
  x * y == y * x

testMulAssociative :: IO ()
testMulAssociative = quickCheck .
  whenFail (putStrLn "not associative") $
  property $
  \ (x :: Integer)
    (y :: Integer)
    (z :: Integer) ->
    mulAssociative x y z
  -- (\x y z-> mulAssociative x y z)

testMulCommutative :: IO ()
testMulCommutative = quickCheck $
  forAll (plusGen :: Gen Integer) mulCommutative
  -- (\x y -> mulCommutative x y)

integralGen :: (Arbitrary a, Integral a, Eq a, Show a) => Gen a
integralGen = do
  a <- arbitrary
  return a

nonZeroIntegralGen :: (Arbitrary a, Integral a, Eq a, Show a) => Gen a
nonZeroIntegralGen = do
  a <- arbitrary
  if a == 0
    then return $ a + 1
    else return a

posIntegralGen :: (Arbitrary a, Integral a, Eq a, Show a) => Gen a
posIntegralGen = do
  a <- arbitrary
  case compare a 0 of
    LT -> return $ negate a
    EQ -> return 1
    GT -> return a

quotRemProp :: (Integral a, Eq a) => a -> a -> Bool
quotRemProp x y =
  (quot x y) * y + (rem x y) == x

divModProp :: (Integral a, Eq a) => a -> a -> Bool
divModProp x y =
  (div x y) * y + (mod x y) == x

testQuotRem :: IO ()
testQuotRem = quickCheck $
  forAll (integralGen :: Gen Integer) $ \numerator ->
  forAll (nonZeroIntegralGen :: Gen Integer) $ \denominator ->
  quotRemProp numerator denominator

testDivMod :: IO ()
testDivMod = quickCheck $
  forAll (integralGen :: Gen Integer) $ \numerator ->
  forAll (nonZeroIntegralGen :: Gen Integer) $ \denominator ->
  divModProp numerator denominator

powAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

powCommutative :: (Integral a, Eq a) => a -> a -> Bool
powCommutative x y =
  x ^ y == y ^ x

testPowAssociative :: IO ()
testPowAssociative = quickCheck . verbose .
  whenFail (putStrLn "not associative") $
  forAll (posIntegralGen :: Gen Integer) $ \x ->
  forAll (posIntegralGen :: Gen Integer) $ \y ->
  forAll (posIntegralGen :: Gen Integer) $ \z ->
  powAssociative x y z

testPowCommutative :: IO ()
testPowCommutative = quickCheck . verbose .
  whenFail (putStrLn "not commutative") $
  forAll (posIntegralGen :: Gen Integer) $ \x ->
  forAll (posIntegralGen :: Gen Integer) $ \y ->
  powCommutative x y


testReverseTwice :: IO ()
testReverseTwice = quickCheck $
  forAll (arbitrary :: Gen String)
  (\s -> (reverse . reverse) s == id s)

prop_dollarDef :: Property
prop_dollarDef =
  property $ \(Fn (f :: Integer -> Integer)) x ->
  (f $ x) === f x

prop_dollarCompose :: Property
prop_dollarCompose =
  property $
   \(Fn (f :: Integer -> Integer))
    (Fn (g :: Integer -> Integer))
    y ->
      (f . g) y === (\x -> f (g x)) y

testDollarDef :: IO ()
testDollarDef = quickCheck .verbose $
  prop_dollarDef

testDollarCompose :: IO ()
testDollarCompose = quickCheck .verbose $
  prop_dollarCompose


testFoldrCons :: IO ()
testFoldrCons = quickCheck . verbose $
  property $
    \ (xs :: [Integer])
      (z  :: [Integer])
      ->
        (foldr (:)) z xs == (++) xs z

testFoldrConcat :: IO ()
testFoldrConcat =  quickCheck . verbose $
  property $
    \ (xs :: [[Integer]]) ->
      (foldr (++) []) xs == concat xs

nonEmptyIntegerListGen :: Int -> Gen [Integer]
nonEmptyIntegerListGen n = do
  a <- arbitrary
  if length a >= n
    then return a
    else return $ a ++ [1..(toInteger n)]

testPartialLength :: IO ()
testPartialLength = quickCheck . verbose $
  forAll (posIntegralGen :: Gen Int) $ \n ->
  forAll (nonEmptyIntegerListGen n :: Gen [Integer]) $ \xs ->
    f n xs == n
    where f m ys = length (take m ys)
