{-# LANGUAGE ScopedTypeVariables #-}

module MonoidEx where

import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = mempty `mappend` x == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = x `mappend` mempty == x


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = do
    return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivial :: IO ()
testTrivial = do
  let
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mri = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)


newtype Identity a =
  Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mappend = (<>)
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentAssoc a = Identity a -> Identity a -> Identity a -> Bool

testIdentity :: IO ()
testIdentity = do
  let
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mri = monoidRightIdentity
  quickCheck (sa :: IdentAssoc String)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mri :: Identity String -> Bool)


data Two a b =
  Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mappend = (<>)
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

testTwo :: IO ()
testTwo = do
  let
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mri = monoidRightIdentity
  quickCheck (sa :: TwoAssoc String String)
  quickCheck (mli :: Two String String -> Bool)
  quickCheck (mri :: Two String String -> Bool)


newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary =
    frequency [ (1, return $ BoolConj True)
              , (1, return $ BoolConj False) ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

testBoolConj :: IO ()
testBoolConj = do
  let
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mri = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mri :: BoolConj -> Bool)


newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _ = BoolDisj True

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [ (1, return $ BoolDisj True)
              , (1, return $ BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

testBoolDisj :: IO ()
testBoolDisj = do
  let
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mri = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mri :: BoolDisj -> Bool)



newtype Combine a b =
  Combine {unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> (f x) <> (g x))

instance (Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine (\x -> mempty)

combAssoc :: (Eq b, Monoid b) =>
  Combine a b -> Combine a b -> Combine a b -> a -> Bool
combAssoc a b c x = (unCombine (a `mappend` (b `mappend` c)) $ x) == (unCombine ((a `mappend` b) `mappend` c) $ x)

combLeftIdentity a x = (unCombine (mempty `mappend` a) $ x) == (unCombine a $ x)
combRightIdentity a x = (unCombine (a `mappend` mempty) $ x) == (unCombine a $ x)

testCombine :: IO ()
testCombine = do
  quickCheck . property $
    \ (Fn (f :: Integer -> Sum Integer))
      (Fn (g :: Integer -> Sum Integer))
      (Fn (h :: Integer -> Sum Integer))
      (x :: Integer) ->
      combAssoc (Combine f) (Combine g) (Combine h) x
  quickCheck . property $
    \ (Fn (f :: Integer -> Sum Integer))
      (x :: Integer) ->
      combLeftIdentity (Combine f) x
  quickCheck . property $
    \ (Fn (f :: Integer -> Sum Integer))
      (x :: Integer) ->
      combRightIdentity (Combine f) x


newtype Comp a =
  Comp { unComp :: (a -> a)}

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (\x -> (f x) <> (g x))

instance (Monoid a) => Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp (\x -> mempty)

compAssoc :: (Eq a, Monoid a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc a b c x = ((unComp (a `mappend` (b `mappend` c)) $ x)) == ((unComp ((a `mappend` b) `mappend` c) $ x))

compLeftIdentity a x = (unComp (mempty `mappend` a) $ x) == (unComp a $ x)
compRightIdentity a x = (unComp (a `mappend` mempty) $ x) == (unComp a $ x)

testComp :: IO ()
testComp = do
  quickCheck . property $
    \ (Fn (f :: Sum Integer -> Sum Integer))
      (Fn (g :: Sum Integer -> Sum Integer))
      (Fn (h :: Sum Integer -> Sum Integer))
      (x :: Sum Integer) ->
      compAssoc (Comp f) (Comp g) (Comp h) x
  quickCheck . property $
    \ (Fn (f :: Sum Integer -> Sum Integer))
      (x :: Sum Integer) ->
      compLeftIdentity (Comp f) x
  quickCheck . property $
    \ (Fn (f :: Sum Integer -> Sum Integer))
      (x :: Sum Integer) ->
      compRightIdentity (Comp f) x


newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem (\x -> (fst (f x) <> fst (g x), snd (f (snd (g x)))))

instance Monoid a => Monoid (Mem s a) where
  mappend = (<>)
  mempty = Mem (\x -> (mempty, x))

f' = Mem $ \s -> ("hi", s + 1)

checkMem = do
  let
    rmzero = runMem mempty 0
    rmleft = runMem (f' <> mempty) 0
    rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright;
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0


memAssoc :: (Eq a, Monoid a, Eq s) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
memAssoc a b c x = ((runMem (a `mappend` (b `mappend` c)) $ x)) == ((runMem ((a `mappend` b) `mappend` c) $ x))

memLeftIdentity f x = (runMem (mempty `mappend` f) $ x) == (runMem f $ x)
memRightIdentity f x = (runMem (f `mappend` mempty) $ x) == (runMem f $ x)

testMem :: IO ()
testMem = do
  quickCheck . property $
    \ (Fn (f :: Int -> (String, Int)))
      (Fn (g :: Int -> (String, Int)))
      (Fn (h :: Int -> (String, Int)))
      (x :: Int) ->
      memAssoc (Mem f) (Mem g) (Mem h) x
  quickCheck . property $
    \ (Fn (f :: Int -> (String, Int)))
      (x :: Int) ->
      memLeftIdentity (Mem f) x
  quickCheck . property $
    \ (Fn (f :: Int -> (String, Int)))
      (x :: Int) ->
      memRightIdentity (Mem f) x
