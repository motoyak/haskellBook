module Interchange where

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
       -> Maybe (a -> b)
       -> Maybe b
mApply = (<*>)

mResult = pure ($ 2) `mApply` Just (+2)