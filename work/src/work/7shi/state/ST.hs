module ST where

import Control.Monad
import Control.Monad.ST
import Data.STRef


sum' xs = runST $ do
  v <- newSTRef 0
  forM_ xs $ \i ->
    modifySTRef v (+ i)
  readSTRef v

main :: IO ()
main = do
  print $ sum' [1..100]
  let a = do
      b <- newSTRef 1
      modifySTRef b (+ 1)
      readSTRef b
  print $ runST a
  let c = return 1 :: ST s Int
  print $ runST c