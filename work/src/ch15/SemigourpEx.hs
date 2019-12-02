{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemigroupEx where

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Function

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen
    where trivialGen = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivial :: IO ()
testTrivial = quickCheck (semigroupAssoc :: TrivAssoc)


newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance Arbitrary a  => Arbitrary (Identity a) where
  arbitrary = identityGen
    where
      identityGen = do
        a <- arbitrary
        return (Identity a)

type IdentAssoc a =
  (Identity a) -> (Identity a) -> (Identity a) -> Bool

testIdentity :: IO ()
testIdentity = quickCheck (semigroupAssoc :: (IdentAssoc String))


data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x<>x') (y<>y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen
    where
      twoGen = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

testTwo :: IO ()
testTwo = quickCheck (semigroupAssoc :: (TwoAssoc String String))


data Three a b c = Three a b c deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
   Semigroup (Three a b c) where
     (<>) (Three a b c) (Three a' b' c') = Three (a<>a') (b<>b') (c<>c')

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen
      where
        threeGen = do
          a <- arbitrary
          b <- arbitrary
          c <- arbitrary
          return $ Three a b c

type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

testThree :: IO ()
testThree = quickCheck (semigroupAssoc :: ThreeAssoc String String String)


data Four a b c d = Four a b c d deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four a' b' c' d') = Four (a<>a') (b<>b') (c<>c') (d<>d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = fourGen
      where
        fourGen = do
          a <- arbitrary
          b <- arbitrary
          c <- arbitrary
          d <- arbitrary
          return $ Four a b c d

type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool

testFour :: IO ()
testFour = quickCheck (semigroupAssoc :: FourAssoc String String String String)


newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary =
    frequency [ (1, return $ BoolConj True)
              , (1, return $ BoolConj False) ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

testBoolConj :: IO ()
testBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)


newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [ (1, return $ BoolDisj True)
              , (1, return $ BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

testBoolDisj :: IO ()
testBoolDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)


data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) _       (Snd a) = Snd a
  (<>) (Snd a) _       = Snd a
  (<>) (Fst a) (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a)
              , (1, return $ Snd b)]

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

testOr :: IO ()
testOr = quickCheck (semigroupAssoc :: OrAssoc Int String)


newtype Combine a b =
  Combine {unCombine :: (a -> b)} deriving (Eq, Show)

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine f') = Combine (\x -> (f x) <> (f' x))

-- combGen :: (Semigroup b, Arbitrary (Fun a b)) =>  Gen (Combine a b)
-- combGen = do
  -- f <- arbitrary
  -- let f' = applyFun f
  -- return $ Combine f'

-- instance (Semigroup b, Arbitrary (Fun a b)) => Arbitrary (Combine a b) where
  -- arbitrary = combGen

instance Show (a -> b) where
  show f = "Function: a -> b"

instance Eq (a -> b) where
  (==) x y = x == y

-- type CombAssoc a b = (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool
-- testCombine = quickCheck .verbose $ (semigroupAssoc :: CombAssoc Int (Sum Int))

combAssoc :: (Eq b, Semigroup b) =>
  Combine a b -> Combine a b -> Combine a b -> a -> Bool
combAssoc a b c x = (unCombine (a <> (b <> c)) $ x) == (unCombine ((a <> b) <> c) $ x)

testCombine :: IO ()
testCombine = quickCheck . verbose $
  property $
    \ (Fn (f :: Integer -> (Sum Integer)))
      (Fn (g :: Integer -> (Sum Integer)))
      (Fn (h :: Integer -> (Sum Integer)))
      x ->
      combAssoc (Combine f) (Combine g) (Combine h) x

-- f = Combine $ \n -> Sum (n + 1)
-- g = Combine $ \n -> Sum (n - 1)

-- sampleCombGen = sample (combGen :: Gen (Combine Integer (Sum Integer)))
