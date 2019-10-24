module CorrectingSyntax1 where
  x = (+)

  f xs = w `x` 1
      where w = length xs

  myId x = (\x -> x) x

  f' (a, b) = a
