module InText where
  f :: Bool -> Int
  f True = error "blah"
  f False = 0

  f' :: Bool -> Int
  f' False = 0
  f' _ = error $ "*** exception: "
              ++ " this is error"

  f'' :: Bool -> Maybe Int
  f'' False = Just 0
  f'' _ = Nothing
