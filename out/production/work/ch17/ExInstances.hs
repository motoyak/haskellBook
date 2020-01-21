module ExInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type BSI = (Bool, String, Int)
targetBSI :: BSI
targetBSI = undefined

type BSIm = ([Bool], [String], [Int])
targetBSIm :: BSIm
targetBSIm = undefined

-- P.726 Exercises: Write Instances

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')
  
instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')
  
instance Eq a => EqProp (Pair a) where
  (=-=) = eq
  
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

testPair :: IO ()
testPair = do
  quickBatch $ functor (Pair targetBSI targetBSI)
  quickBatch $ applicative (Pair targetBSI targetBSI)
  

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a x) = Two a (f x)
  
instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' x) = Two (a `mappend` a') (f x)
  
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

testTwo :: IO ()
testTwo = do
  quickBatch $ functor (Two targetBSIm targetBSI)
  quickBatch $ applicative (Two targetBSIm targetBSI)
  

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)
  
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' x) = Three (a `mappend` a') (b `mappend` b') (f x)
  
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Monoid a, Monoid b, Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

testThree :: IO ()
testThree = do
  quickBatch $ functor (Three targetBSIm targetBSIm targetBSI)
  quickBatch $ applicative (Three targetBSIm targetBSIm targetBSI)
  

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x x') = Three' a (f x) (f x')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a `mappend` a') (f x) (f' x')
  
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

testThree' :: IO ()
testThree' = do
  quickBatch $ functor (Three' targetBSIm targetBSI targetBSI)
  quickBatch $ applicative (Three' targetBSIm targetBSI targetBSI)
  

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c x) = Four a b c (f x)
  
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' x) = Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (f x)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
  
instance (Monoid a, Monoid b, Monoid c, Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

testFour :: IO ()
testFour = do
  quickBatch $ functor (Four targetBSIm targetBSIm targetBSIm targetBSI)
  quickBatch $ applicative (Four targetBSIm targetBSIm targetBSIm targetBSI)
  

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c x) = Four' a b c (f x)
  
instance Monoid a => Applicative (Four' a) where
  pure  = Four' mempty mempty mempty
  (<*>) (Four' a b c f) (Four' a' b' c' x) = Four' (a `mappend` a') (b `mappend` b') (c `mappend` c') (f x)
  
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d
 
testFour' :: IO ()
testFour' = do
  quickBatch $ functor (Four' targetBSIm targetBSIm targetBSIm targetBSI)
  quickBatch $ applicative (Four' targetBSIm targetBSIm targetBSIm targetBSI)
  
main :: IO ()
main = do
  putStrLn "Answer 1: Pair a"
  testPair
  putStrLn ""
  putStrLn "Answer 2: Two a b"
  testTwo
  putStrLn ""
  putStrLn "Answer 3: Three a b c"
  testThree
  putStrLn ""
  putStrLn "Answer 4: Three' a b"
  testThree'
  putStrLn ""
  putStrLn "Answer 5: Four a b c d"
  testFour
  putStrLn ""
  putStrLn "Answer 6: Four' a b"
  testFour'