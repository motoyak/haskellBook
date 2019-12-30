module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
  
instance Semigroup (List a) where
  (<>) = append

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)
  
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
  
append :: List a -> List a -> List a
append xs Nil = xs
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq
  
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (3, return $ Cons h t)
            , (1, return Nil)]


newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)
  
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take' 3000 l
      ys' = let (ZipList' l) = ys
            in take' 3000 l

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Semigroup (ZipList' a) where
  (<>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)

instance Monoid (ZipList' a) where
  mempty = ZipList' Nil
  mappend = (<>)
  
instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs
    
repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a->b->c) -> List a -> List b -> List c
zipWith' _ _ Nil = Nil
zipWith' _ Nil _ = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith' ($) fs xs)

zl' x = ZipList' $ toMyList x
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
  return $ ZipList' (Cons h t)

main :: IO ()
main = do
  putStrLn "[Test: List]"
  quickBatch $ semigroup (Cons (undefined::Char) Nil, undefined::Int)
  quickBatch $ monoid (Cons (undefined::Int) Nil)
  quickBatch $ functor (Cons trigger Nil)
  quickBatch $ applicative (Cons trigger Nil)
  putStrLn "[Test: ZipList']"
  quickBatch $ semigroup (ZipList' (Cons (undefined::Char) Nil), undefined::Int)
  quickBatch $ monoid (ZipList' (Cons (undefined :: Int) Nil))
  quickBatch $ functor (ZipList' (Cons trigger Nil))
  quickBatch $ applicative (ZipList' (Cons trigger Nil))

