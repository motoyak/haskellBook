{-# LANGUAGE RankNTypes#-}

module Hole1 where

data PairHole a b
  = HoleFst b
  | HoleSnd a

data PairZipper a b c =
  PZ c
     (PairHole a b)

focusFst :: (a, b) -> PairZipper a b a
focusFst (x, y) = PZ x (HoleFst y)

focusSnd :: (a, b) -> PairZipper a b b
focusSnd (x, y) = PZ y (HoleSnd x)

unfocusFst :: PairZipper a b a -> (a, b)
unfocusFst (PZ a (HoleFst b)) = (a, b)

unfocusSnd :: PairZipper a b b -> (a, b)
unfocusSnd (PZ b (HoleSnd a)) = (a, b)

view' :: PairZipper a b c -> c
view' (PZ c _) = c

over' :: (c -> d) -> PairZipper a b c -> PairZipper a b d
over' f (PZ c l) = PZ (f c) l

data Focused t a b = Focused
  { focused :: a
  , rebuild :: b -> t
  }

type Focuser s t a b = s -> Focused t a b

unfocus :: Focused s a a -> s
unfocus (Focused focused rebuild) = rebuild focused

view :: Focuser s t a b -> s -> a
view l s = focused (l s)

over :: Focuser s t a b -> (a -> b) -> s -> t
over l f s =
  let Focused focused rebuild = l s
   in rebuild (f focused)
   
_1 :: Focuser (a,b) (c,b) a c
_1 (a,b) = Focused a (\c -> (c,b))

_2 :: Focuser (a,b) (a,c) b c
_2 (a,b) = Focused b (\c -> (a,c))


focusHead :: Focuser [a] [a] a a
focusHead (x:xs) = Focused x (:xs)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t