{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

import Test.QuickCheck

functorIdentity ::
  (Functor f, Eq (f a)) =>
     f a
  -> Bool
functorIdentity f = fmap id f == f

functorCompose ::
  (Functor f, Eq (f c)) =>
     (a -> b)
  -> (b -> c)
  -> f a
  -> Bool
functorCompose f g x =
  fmap`` g (fmap f x) == fmap (g . f) x

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ False' a),
               (1, return $ True' b)]

testBoolAndSomethingElse = do
  quickCheck . property $
    \ (x :: BoolAndSomethingElse Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: BoolAndSomethingElse Int)
    ->
    functorCompose f g x


data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ Falsish),
               (1, return $ Truish a)]

testBoolAndMaybeSomethingElse = do
  quickCheck . property $
    \ (x :: BoolAndMaybeSomethingElse Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: BoolAndMaybeSomethingElse Int)
    ->
    functorCompose f g x


data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ First a),
               (1, return $ Second b)]

testSum = do
  quickCheck . property $
    \ (x :: Sum String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Sum String Int)
    ->
    functorCompose f g x


data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a c b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    frequency [(1, return $ DeepBlue a c),
               (1, return $ Something b)]
testCompany = do
  quickCheck . property $
    \ (x :: Company String Int Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Company String Int Int)
    ->
    functorCompose f g x


data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    frequency [(1, return $ L a b a'),
               (1, return $ R b a b')]
testMore = do
  quickCheck . property $
    \ (x :: More String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: More String Int)
    ->
    functorCompose f g x

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ (Finance) = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Finance),
               (1, return $ Desk a),
               (1, return $ Bloor b)]
testQuant = do
  quickCheck . property $
    \ (x :: Quant String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Quant String Int)
    ->
    functorCompose f g x


data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

testK = do
  quickCheck . property $
    \ (x :: K String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: K String Int)
    ->
    functorCompose f g x

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)


instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K b)

testFlipK = do
  quickCheck . property $
    \ (x :: Flip K String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Flip K String Int)
    ->
    functorCompose f g x


data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

testGoatee = do
  quickCheck . property $
    \ (x :: EvilGoateeConst String Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: EvilGoateeConst String Int)
    ->
    functorCompose f g x

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

testLiftItOut = do
  quickCheck . property $
    \ (x :: Maybe Int)
    ->
    functorIdentity $ LiftItOut x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Maybe Int)
    ->
    functorCompose f g (LiftItOut x)


data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

testParappa = do
  quickCheck . property $
    \ (x :: Maybe Int)
      (y :: [Int])
    ->
    functorIdentity $ DaWrappa x y
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Maybe Int)
      (y :: [Int])
    ->
    functorCompose f g $ DaWrappa x y


data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

testIgnoreOne = do
  quickCheck . property $
    \ (x :: Maybe Int)
      (y :: [Int])
    ->
    functorIdentity $ IgnoringSomething x y
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Maybe Int)
      (y :: [Int])
    ->
    functorCompose f g $ IgnoringSomething x y


data Notorius g o a t =
  Notorius (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorius g o a) where
  fmap f (Notorius x y z) = Notorius x y (fmap f z)

testNotorius = do
  quickCheck . property $
    \ (x :: Maybe Char)
      (y :: Maybe String)
      (z :: Maybe Int)
    ->
    functorIdentity $ Notorius x y z
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: Maybe Char)
      (y :: Maybe String)
      (z :: Maybe Int)
    ->
    functorCompose f g $ Notorius x y z


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    frequency [(1, return $ Nil),
               (99, return $ Cons a as)]

testList = do
  quickCheck . property $
    \ (x :: List Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: List Int)
    ->
    functorCompose f g x


data GoatLoad a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLoad a)
              (GoatLoad a)
              (GoatLoad a)
  deriving (Eq, Show)

instance Functor GoatLoad where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance Arbitrary a => Arbitrary (GoatLoad a) where
  arbitrary = do
    a <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    frequency [(1, return NoGoat),
               (1, return $ OneGoat a),
               (1, return $ MoreGoats x y z)]

testGoatLoad = do
  quickCheck . property $
    \ (x :: GoatLoad Int)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: GoatLoad Int)
    ->
    functorCompose f g x


data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read (f . g)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print x y) = "Print: [" <> show x <> "] " <> show y
  show (Read f) = "Read"

instance Eq a => Eq (TalkToMe a) where
  (==) Halt Halt = True
  (==) (Print x y) (Print x' y') = y == y'
  (==) (Read x) (Read x') = feq x x'
    where
    feq f g =
      let dummy = "abc"
      in f dummy == g dummy

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = do
    a <- arbitrary
    x <- arbitrary
    y <- arbitrary -- Gen (String -> a)
    frequency [(1, return Halt),
               (1, return $ Print x a),
               (1, return $ Read y)]

testTalkToMe = do
  quickCheck . property $
    \ (x :: TalkToMe String)
    ->
    functorIdentity x
  quickCheck . property $
    \ (Fn (f :: Int -> Int))
      (Fn (g :: Int -> Int))
      (x :: TalkToMe Int)
    ->
    functorCompose f g x


