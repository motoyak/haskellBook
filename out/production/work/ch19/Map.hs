module Map where

import qualified Data.Map as M
--import Data.Monoid

f = M.fromList [('a', 1)]
g = M.fromList [('b', 2)]

fg = f <> g

