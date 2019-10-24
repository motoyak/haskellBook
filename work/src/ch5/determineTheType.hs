{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
  example = 1

  q1_a = (* 9) 6
  q1_b = head [(0,"doge"),(1,"kitteh")]
  q1_c = head [(0::Integer, "doge"),(1,"kitteh")]
  q1_d = if False then True else False
  q1_e = length [1,2,3,4,5]
  q1_f = (length [1,2,3,4]) > (length "TACOCAT")

  x = 5
  y = x + 5
  w = y * 10
  z y = y * 10
  f = 4 / y

  x' = "Julie"
  y' = " <3"
  z' = "Haskell"
  f' = x' ++ y' ++ z'

  bigNum = (^) 5 $ 10
  bigNum' = (^) 5
  wahoo = bigNum' $ 10

  x2 = print
  y2 = print "woohoo!"
  z2 = x2 "hello world"


  a = (+)
  b = a 5
  c = b 10
  d = b 100

  a' = 12 + b'
  b' = 10000 * c'
  c' = 23
