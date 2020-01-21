module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TargetTypes = (String, Char, [String], Int, Int)

-- 1
data Constant a b =
  Constant b
  deriving(Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = do
    b <- arbitrary
    return $ Constant b

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

testConstant :: IO ()
testConstant = quickBatch $ foldable (undefined :: Constant Int TargetTypes)


-- 2
data Two a b =
  Two a b
  deriving(Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two x y) = f y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

testTwo :: IO ()
testTwo = quickBatch $ foldable (undefined :: Two Int TargetTypes)


-- 3
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three x1 x2 x3) = f x3 z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThree :: IO ()
testThree = quickBatch $ foldable (undefined :: Three Int Float TargetTypes)


-- 4
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' x y1 y2) = f y2 z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2aaa

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

testThree' :: IO ()
testThree' = quickBatch $ foldable (undefined :: Three' Int TargetTypes)


-- 5
data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' a x1 x2 x3) = f x3 z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return $ Four' a b1 b2 b3

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

testFour' :: IO ()
testFour' = quickBatch $ foldable (undefined :: Four' Int TargetTypes)


-- thinking cap time
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
       => (a -> Bool) -> t a -> f a
filterF f = foldr (\x y -> if f x then (pure x) `mappend` y else y) mempty
