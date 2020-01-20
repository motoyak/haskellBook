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
data Constant' a b = Constant'
  { getConstant' :: a
  } deriving (Eq, Show)

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
  arbitrary = do
    a <- arbitrary
    return $ Constant' a

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
        frequency [ (3, return $ Cons h t)
                  , (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList = do
  quickBatch $ semigroup (undefined :: List Char, undefined::Int)
  quickBatch $ monoid (undefined :: List Int)
  quickBatch $ functor (undefined :: List TriggerType)
  quickBatch $ applicative (undefined :: List TriggerType)
  quickBatch $ foldable (undefined :: List TargetTypesFoldable)
  quickBatch $ traversable (undefined :: List TriggerType)