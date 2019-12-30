module InText6 where

replaceWithP = const 'p'

tossEmOne = fmap (+1) negate
tossEmOne' = (+1) . negate

n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]

-- tripFmap = fmap . fmap . fmap

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
-- b :: m -> n, c :: f m -> f n, a :: x -> y, b :: g x -> g y
-- m -> n :: f x -> f y =>  m :: g x, n :: g y
-- fmap . fmap :: a -> c = (x -> y) -> (f m -> f n)
-- =>
-- fmap . fmap :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)


ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]
