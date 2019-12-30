module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)
  
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take 3000 l
      ys' = let (ZipList' l) = ys
            in take 3000 l
            
instance Semigroup (ZipList' a) where
  (<>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)

instance Monoid (ZipList' a) where
  mempty = ZipList' []
  mappend = (<>)
  
instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs
    
instance Applicative ZipList' where
  pure x = ZipList' xs
    where xs = x : xs
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zip' fs xs)

zip' :: [a->b] -> [a] -> [b]
zip' [] _ = []
zip' _ [] = []
zip' (f:fs) (x:xs) = f x : zip' fs xs

zl' = ZipList'
z = zl' [(+9), (*2), (+8)]
z' = zl' [1..3]
test1 = z <*> z'
z'' = pure 1
test2 = z <*> z''
z''' = zl' [1,2]
test3 = pure id <*> z'''

trigger :: (Bool, Char, Int)
trigger = undefined

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = genZipList'

genZipList' :: Arbitrary a => Gen (ZipList' a)
genZipList' = do
  h <- arbitrary
  t <- genList
  return $ ZipList' (h:t)

genList :: Arbitrary a => Gen [a]
genList = do
  h <- arbitrary
  t <- genList
  return $ h:t

main :: IO ()
main = do
  quickBatch $ semigroup (ZipList' [undefined::Char], undefined::Int)
  quickBatch $ monoid (ZipList' [undefined :: Int])
  quickBatch $ functor (ZipList' [trigger])
  quickBatch $ applicative (ZipList' [trigger])