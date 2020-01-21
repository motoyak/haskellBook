module LetsWriteCode where

  tensDigit :: Integral a => a -> a
  tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

  tensDigit' :: Integral a => a -> a
  tensDigit' x = snd . flip divMod 10 . fst . flip divMod 10 .abs $ x

  hunsD x = snd . flip divMod 10 . fst . flip divMod 100 .abs $ x

  foldBool :: a -> a -> Bool -> a
  foldBool x y cond =
    case cond of
      True -> y
      False -> x

  foldBool' :: a -> a -> Bool -> a
  foldBool' x y cond
    | cond = y
    | otherwise = x

  foldBool'' :: a -> a -> Bool -> a
  foldBool'' x y cond =
    if cond
      then y
      else x

  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x _ False = x
  foldBool3 _ y True = y

  g :: (a -> b) -> (a, c) -> (b, c)
  g f (x, y) = (f x, y)
-- > g (\xs -> head xs) ("abc",10)
-- ('a',10)
