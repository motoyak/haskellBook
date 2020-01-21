module LawQuickCheck where

import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type S = String
type B = Bool

testAssoc = quickCheck (monoidAssoc :: S -> S -> S -> B)
testLeftIdentity = quickCheck (monoidLeftIdentity :: S -> B)
testRightIdentity = quickCheck (monoidRightIdentity :: S -> B)
