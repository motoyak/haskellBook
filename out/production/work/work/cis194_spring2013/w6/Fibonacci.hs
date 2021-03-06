{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fibs :: Integer -> Integer
fibs n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibs (n-1) + fibs (n-2)

fibs1 :: [Integer]
fibs1 = map fibs [0..]

fibs2 :: [Integer]
fibs2 = [0,1] ++ go [0,1]
  where
    go acc = (head acc + last acc) : go [last acc, head acc + last acc]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show stream = "Stream [" ++ go 20 stream ++ "]"
    where
      go n (Cons x xs)
        | n == 0 = ""
        | otherwise = show x ++ (if n > 1 then "," else ",...") ++ go (n-1) xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f $ f seed

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
interleaveStreams f (Cons x xs) (Cons y ys) = Cons (f x y) $ interleaveStreams f xs ys

pnats :: Stream Integer
pnats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap divisibilityTest pnats
  where
    divisibilityTest n
      | n `mod` 2 /= 0 = 0
      | otherwise = lp2DivN n 1 1
    lp2DivN n acc lp2
      | 2^acc > n = lp2
      | n `mod` 2^acc == 0 = lp2DivN n (acc+1) acc
      | otherwise = lp2DivN n (acc+1) lp2

zeros :: Stream Integer
zeros = streamRepeat 0

logBases :: Stream Integer
logBases = streamMap (\n -> if n `mod` 2 == 0 then floor . logBase 2 . fromInteger $ n else 0) pnats

logBacks :: Stream Integer
logBacks = streamMap (\n -> if n == 0 then 0 else 2^n) logBases

evens :: Stream Integer
evens = interleaveStreams (\x y -> if x `mod` 2 == 0 then x else y) pnats zeros

ruler' :: Stream Integer
ruler' = interleaveStreams (\x y -> if x==0 then 0 else floor . logBase 2 . fromIntegral . gcd x $ y) evens logBacks



x :: Stream Integer
x = streamMap (\n -> if n == 1 then 1 else 0) nats

streamPrepend :: a -> Stream a-> Stream a
streamPrepend = Cons

instance Num (Stream Integer) where
  fromInteger n = streamMap (\m -> if m == 0 then n else 0) nats
  negate = streamMap negate
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) (Cons b bs) = Cons (a * b) (streamMap (a*) bs + as*Cons b bs)

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs)
    | b==0 = as / bs
    | otherwise =
      let q = Cons a as / Cons b bs
      in
      Cons (a `myDiv` b) (streamMap (`myDiv` b) $ as - q*bs)
      where
        myDiv x y
         | y == 0 = 0
         | otherwise = signum x * signum y * div (abs x) (abs y)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) =
    Matrix (a00*b00 + a10*b01) (a01*b00 + a11*b01) (a00*b10 + a10*b11) (a01*b10 + a11*b11)

instance Show Matrix where
  show (Matrix a00 a01 a10 a11) = "|" ++ show a00 ++ " " ++ show a10 ++ "|\n" ++ "|" ++ show a01 ++ " " ++ show a11 ++ "|"

pickMatrix01 :: Matrix -> Integer
pickMatrix01 (Matrix _ x _ _) = x

fib4 :: Integer -> Integer
fib4 n
  | n ==0 = 0
  | otherwise =
    let fibSeed = Matrix 1 1 1 0
    in
    pickMatrix01 $ fibSeed ^ n
