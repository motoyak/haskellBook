{-# LANGUAGE RankNTypes #-}

module BadNat where

type Nat f g a = forall a . f a -> g a

maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

degenerateMtl :: Num a => Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]
--degenerateMtl (Just a) = [a]
