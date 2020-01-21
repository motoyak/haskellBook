module InText5 where

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  -- fmap _ ItDoesnt = ItDoesnt
  -- fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap _ WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)


data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

u = "uncle"
oneWhoKnocks = Heisenberg 0 u
ex1 = fmap (++" Jesse") oneWhoKnocks

f' = ((++ " Jesse").(++" lol"))
ex2 = fmap f' oneWhoKnocks

f = (++" Jesse")
g = (++" lol")

ex3 = fmap f . fmap g $ oneWhoKnocks
ex4 = fmap (f.g) oneWhoKnocks


data CountingGood a =
  Heisenberg' Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg' n a) = Heisenberg' n (f a)

oneWhoKnocks' = Heisenberg' 0 u
ex5 = fmap f . fmap g $ oneWhoKnocks'
ex6 = fmap (f.g) oneWhoKnocks'
