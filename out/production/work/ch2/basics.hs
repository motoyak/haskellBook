#!/usr/bin/env stack
-- stack script --resolver lts-14.7
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, wait, cancel)

sleep :: Int -> IO ()
sleep n = threadDelay $ 1000*1000 * n

sleepAndPut :: Int -> IO ()
sleepAndPut n = do
  sleep n
  putStrLn $ show n <> " second later"

waitEnter :: IO ()
waitEnter = do
  c <- getChar
  if c == '\n' then pure () else waitEnter

waitEnterAndCancel :: Async a -> IO ()
waitEnterAndCancel a = do
  waitEnter
  cancel a

main :: IO ()
main = do
    putStrLn "start"
    a1 <- async $ sleepAndPut 100
    async $ waitEnterAndCancel a1
    wait a1
    putStrLn "done"
