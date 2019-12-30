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

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f = concat' . fmap f

testF x = Cons x (Cons 9 Nil)

toMyList :: [a] -> List a
toMyList = foldr Cons Nil
testX = toMyList [1,2,3]

instance Eq a => EqProp (List a) where
  (=-=) = eq
  
trigger :: (String, Char, Int)
trigger = undefined

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (3, return $ Cons h t)
            , (1, return Nil)]

main :: IO ()
main = do
  quickBatch $ semigroup (Cons (undefined::Char) Nil, undefined::Int)
  quickBatch $ monoid (Cons (undefined::Int) Nil)
  quickBatch $ functor (Cons trigger Nil)
  quickBatch $ applicative (Cons trigger Nil)