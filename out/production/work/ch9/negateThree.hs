module FordBool where

import Data.Bool

negateThree :: (Num a, Eq a) => [a] -> [a]
negateThree = map (\x -> bool x (-x) (x==3))
