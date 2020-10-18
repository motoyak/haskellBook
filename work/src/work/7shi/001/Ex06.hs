module Ex06 where

perpPoint :: (Double, Double) -> (Double, Double, Double) -> (Double, Double)
perpPoint (p, q) (a, b, c) = (x,y)
  where
    x = (a*c+b*d) / (a*a+b*b)
    y = (b*c-a*d) / (a*a+b*b)
    d = b*p - a*q
