module Destructive where

import Data.IORef
import Control.Monad

sum' xs = do
  v <- newIORef 0
  forM_ xs $ \i ->
    modifyIORef v (+ i)
  readIORef v

main :: IO ()
main = do
  a <- newIORef 1
  writeIORef a 2
  print =<< readIORef a
  print =<< sum' [1..100]