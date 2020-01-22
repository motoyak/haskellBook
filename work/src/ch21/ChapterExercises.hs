{-# LANGUAGE FlexibleContexts #-}

module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TriggerType = (Int, Int, [Int])

type TargetTypesFoldable = (String, Char, [String], Int, Int)

-- Identity
data Identity' a =
  Identity' a
  deriving (Eq, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' (f x)

instance Foldable Identity' where
  foldMap f (Identity' x) = f x
  foldr f z (Identity' x) = f x z

instance Traversable Identity' where
  traverse f (Identity' x) = Identity' <$> f x

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => EqProp (Identity' a) where
  (=-=) = eq

testIdentity' = do
  quickBatch $ traversable (undefined :: Identity' TriggerType)

-- Constant
data Constant' a b =
  Constant'
    { getConstant' :: a
    }
  deriving (Eq, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' x) = Constant' x

instance Monoid a => Applicative (Constant' a) where
  pure _ = Constant' mempty
  (<*>) (Constant' x) (Constant' y) = Constant' (x `mappend` y)

instance Foldable (Constant' a) where
  foldMap _ (Constant' _) = mempty

instance Traversable (Constant' a) where
  traverse f (Constant' x) = pure (Constant' x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant' a b) where
  arbitrary = Constant' <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant' a b) where
  (=-=) = eq

testConstant' = do
  quickBatch $ functor (undefined :: Constant' TriggerType TriggerType)
  quickBatch $ applicative (undefined :: Constant' [Char] TriggerType)
  quickBatch $ foldable (undefined :: Constant' TriggerType TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Constant' TriggerType TriggerType)

-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure = Yep
  (<*>) Nada _ = Nada
  (<*>) _ Nada = Nada
  (<*>) (Yep f) (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (1, return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

testOptional = do
  quickBatch $ functor (undefined :: Optional TriggerType)
  quickBatch $ applicative (undefined :: Optional TriggerType)
  quickBatch $ foldable (undefined :: Optional TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Optional TriggerType)

-- List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) = append
    where
      append xs Nil = xs
      append Nil ys = ys
      append (Cons x xs) ys = Cons x $xs `append` ys

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons fh ft) ys = (fh <$> ys) <> (ft <*> ys)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList
    where
      genList = do
        h <- arbitrary
        t <- genList
        frequency [(3, return $ Cons h t), (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList = do
  quickBatch $ semigroup (undefined :: List Char, undefined :: Int)
  quickBatch $ monoid (undefined :: List Int)
  quickBatch $ functor (undefined :: List TriggerType)
  quickBatch $ applicative (undefined :: List TriggerType)
  quickBatch $ foldable (undefined :: List TargetTypesFoldable)
  quickBatch $ traversable (undefined :: List TriggerType)

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a1 b1 f) (Three a2 b2 x) = Three (a1 `mappend` a2) (b1 `mappend` b2) (f x)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
  traverse f (Three a b x) = Three a b <$> f x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThree = do
  quickBatch $ functor (undefined :: Three TriggerType TriggerType TriggerType)
  quickBatch $ applicative (undefined :: Three ([Bool], [String], [Int]) ([Bool], [String], [Int]) TriggerType)
  quickBatch $ foldable (undefined :: Three String String TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Three TriggerType TriggerType TriggerType)

-- Pair
data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a x) = Pair a (f x)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  (<*>) (Pair a1 f1) (Pair a2 x2) = Pair (a1 `mappend` a2) (f1 x2)

instance Foldable (Pair a) where
  foldMap f (Pair a x) = f x

instance Traversable (Pair a) where
  traverse f (Pair a x) = Pair a <$> f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

testPair = do
  quickBatch $ functor (undefined :: Pair TriggerType TriggerType)
  quickBatch $ applicative (undefined :: Pair ([Bool], [String], [Int]) TriggerType)
  quickBatch $ foldable (undefined :: Pair String TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Pair TriggerType TriggerType)

-- Big
data Big a b =
  Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a x1 x2) = Big a (f x1) (f x2)

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  (<*>) (Big a1 f1 g1) (Big a2 x2 y2) = Big (a1 `mappend` a2) (f1 x2) (g1 y2)

instance Foldable (Big a) where
  foldMap f (Big a x y) = f x `mappend` f y

instance Traversable (Big a) where
  traverse f (Big a x y) = Big a <$> f x <*> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Big a b b

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

testBig = do
  quickBatch $ functor (undefined :: Big TriggerType TriggerType)
  quickBatch $ applicative (undefined :: Big ([Bool], [String], [Int]) TriggerType)
  quickBatch $ foldable (undefined :: Big String TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Big TriggerType TriggerType)
 -- Bigger

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a x1 x2 x3) = Bigger a (f x1) (f x2) (f x3)

instance Monoid a => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (<*>) (Bigger a1 f1 f2 f3) (Bigger a2 x1 x2 x3) = Bigger (a1 `mappend` a2) (f1 x1) (f2 x2) (f3 x3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a x1 x2 x3) = f x1 `mappend` f x2 `mappend` f x3

instance Traversable (Bigger a) where
  traverse f (Bigger a x1 x2 x3) = Bigger a <$> f x1 <*> f x2 <*> f x3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Bigger a b b b

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

testBigger = do
  quickBatch $ functor (undefined :: Bigger TriggerType TriggerType)
  quickBatch $ applicative (undefined :: Bigger ([Bool], [String], [Int]) TriggerType)
  quickBatch $ foldable (undefined :: Bigger String TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Bigger TriggerType TriggerType)

-- S
data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (f <$> x) (f y)

instance Applicative n => Applicative (S n) where
  pure x = S (pure x) x
  (<*>) (S f1 f2) (S x1 x2) = S (f1 <*> x1) (f2 x2)

instance Foldable n => Foldable (S n) where
  foldMap f (S x1 x2) = foldMap f x1 `mappend` f x2

instance Traversable n => Traversable (S n) where
  traverse f (S x1 x2) = S <$> traverse f x1 <*> f x2

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

testS = do
  quickBatch $ functor (undefined :: S [] TriggerType)
  quickBatch $ applicative (undefined :: S [] TriggerType)
  quickBatch $ foldable (undefined :: S [] TargetTypesFoldable)
  quickBatch $ traversable (undefined :: S [] TriggerType)

-- Tree
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node x a y) = Node (f <$> x) (f a) (f <$> y)

-- maybe Applicative instance is insane??
instance Applicative Tree where
  pure = Leaf
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Leaf f) (Leaf x) = Leaf (f x)
  (<*>) (Leaf f) (Node l x r) = Node (f <$> l) (f x) (f <$> r)
  (<*>) (Node l1 f r1) (Leaf x)= Leaf (f x)
  (<*>) (Node l1 f r1) (Node l2 x r2 ) = Node (f <$> l2) (f x) (f <$> r2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

--  foldr _ z Empty = z
--  foldr f z (Leaf x) = f x z
--  foldr f z (Node l x r) = f x (foldr f (foldr f z r) l)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree
    where
      genTree = do
        a <- arbitrary
        l <- genTree
        r <- genTree
        frequency [ (1, return Empty)
                  , (1, return $ Leaf a)
                  , (1, return $ Node l a r)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

testTree = do
  quickBatch $ functor (undefined :: Tree TriggerType)
  quickBatch $ foldable (undefined :: Tree TargetTypesFoldable)
  quickBatch $ traversable (undefined :: Tree TriggerType)