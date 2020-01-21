module EnumFrom where

eftBool :: Bool -> Bool -> [Bool]
eftBool start stop = go start stop [start]
  where
    go elm end acc
      | elm == end = acc
      | otherwise = go succElm end $ acc ++ [succElm]
                      where succElm = succ elm

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = go start stop [start]
  where
    go elm end acc
      | elm == end = acc
      | otherwise = go succElm end $ acc ++ [succElm]
                      where succElm = succ elm

eftInt :: Int -> Int -> [Int]
eftInt start stop = go start stop [start]
  where
    go elm end acc
      | elm == end = acc
      | otherwise = go succElm end $ acc ++ [succElm]
                      where succElm = succ elm

eftChar :: Char -> Char -> [Char]
eftChar start stop = go start stop [start]
  where
    go elm end acc
      | elm == end = acc
      | otherwise =
            let succElm = succ elm in
            go succElm end $ acc ++ [succElm]
