module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

mulBySum :: (Eq a, Num a) => a -> a -> a
mulBySum x = go 0
  where
    go sum' i
      | i == 0 = sum'
      | otherwise = go (sum' + x) (i - 1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greter than 1" $ do
      (1 + 1) > (1 :: Int) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` (4 :: Int)
    it "15 divieded by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5 :: Int, 0)
    it "22 divided by 5 is\
       \ 4 reminder 2" $ do
         dividedBy 22 5 `shouldBe` (4 :: Int, 2)
    it "3 multiplied by 2 is 6" $ do
      mulBySum 3 2 `shouldBe` (6 ::Int)
    it "15 multiplied by 4 is 60" $ do
      mulBySum 15 4 `shouldBe` (60 ::Int)
    it "x + 1 is always\
       \ greater than x" $ do
         property $ \x -> x + 1 > (x :: Int)


tete :: IO ()
tete = do
  sample (arbitrary :: Gen Int)
  --sample arbitrary
