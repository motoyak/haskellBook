module InTextReaderMonad where

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor t, Foldable t, Num a) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

--fooBind :: (t2 -> t1) -> (t1 -> t2 -> t) -> t2 -> t
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> t)
fooBind m k = \r -> k (m r) r


