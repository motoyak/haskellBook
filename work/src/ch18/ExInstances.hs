module ExInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TargetTypes = (Bool, String, Int)
target = undefined :: TargetTypes

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
  
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
  
instance Monad Nope where
  return = pure
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope :: IO ()
testNope = do
  quickBatch $ functor (NopeDotJpg :: Nope TargetTypes)
  quickBatch $ applicative (NopeDotJpg :: Nope TargetTypes)
  quickBatch $ monad (NopeDotJpg :: Nope TargetTypes)
  
data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight x) = PRight x
  
instance Applicative (BahEither b) where
  pure = PLeft
  -- caution: order of PRight must keep consistency with monad
  (<*>) (PRight x) _ = PRight x
  (<*>) _ (PRight x) = PRight x
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)
--  (<*>) (PLeft f) x = fmap f x
  
instance Monad (BahEither b) where
  return = pure
  PRight x >>= _ = PRight x
  PLeft x >>= f = f x
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ PLeft a)
              , (1, return $ PRight b)]
              
instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq
  
testBahEither :: IO ()
testBahEither = do
  quickBatch $ functor (undefined :: BahEither TargetTypes TargetTypes)
  quickBatch $ applicative (undefined :: BahEither TargetTypes TargetTypes)
  quickBatch $ monad (undefined :: BahEither TargetTypes TargetTypes)
  
  
-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
  
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) = fmap f

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
  
instance Eq a => EqProp (Identity a) where
  (=-=) = eq
  
testIdentity :: IO ()
testIdentity = do
  quickBatch $ functor (Identity target)
  quickBatch $ applicative (Identity target)
  quickBatch $ monad (Identity target)
  
  
-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons h t) xs = fmap h xs `appendList` (t <*> xs)

appendList :: List a -> List a -> List a
appendList xs Nil = xs
appendList Nil xs = xs
appendList (Cons h t) x = Cons h (t `appendList` x)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons h t >>= f = f h `appendList` (t >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList
  
genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (3, return $ Cons h t)
            , (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = do
  quickBatch $ functor (undefined :: List TargetTypes)
  quickBatch $ applicative (undefined :: List TargetTypes)
  quickBatch $ monad (undefined :: List TargetTypes)