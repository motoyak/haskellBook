module InText where
  bindExp :: Integer -> String
  bindExp x =
    let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

  bindExp' :: Integer -> String
  bindExp' x =
    let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

  funcZ x =
    case x + 1 == 1 of
      True -> "AWESOME"
      False -> "wut"

  pal xs =
    case xs == reverse xs of
      True -> "yes"
      False -> "no"

  pal' xs =
    case y of
      True -> "yes"
      False -> "no"
    where y = xs == reverse xs

  returnLast :: a -> b -> c -> d -> d
  returnLast _ _ _ d = d

  returnLast' :: a -> (b -> (c -> (d -> d)))
  returnLast' _ _ _ d = d

--  returnBroke :: (((a -> b) -> c) -> d) -> d
--  returnBroke _ _ _ d = d

  returnAfterApply :: (a -> b) -> a -> c -> b
  returnAfterApply f a c = f a

  myAbs :: Integer -> Integer
  myAbs x
    | x < 0 = (-x)
    | otherwise = x

  bloodNa :: Integer -> String
  bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

  isRight :: (Num a, Eq a) => a -> a -> a -> String
  isRight a b c
    | a^2 + b^2 == c^2 = "RIGHT ON"
    | otherwise = "not right"

  dogYrs :: Integer -> Integer
  dogYrs x
    | x <= 0 = 0
    | x <= 1 = x * 15
    | x <= 2 = x * 12
    | x <= 4 = x * 8
    | otherwise = x * 6

  avgGrade :: (Fractional a, Ord a) => a -> Char
  avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 059 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

  avgGrade' :: (Fractional a, Ord a) => a -> Char
  avgGrade' x
    | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 059 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

  avgGrade'' :: (Fractional a, Ord a) => a -> Char
  avgGrade'' x
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 059 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

  pal'' xs
    | xs == reverse xs = True
    | otherwise = False

  numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
