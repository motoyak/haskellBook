module Triple where
  triple :: Integer -> Integer
  triple x = x * 3

  triple' x = tripleItYo x
    where tripleItYo :: Integer -> Integer
          tripleItYo y = y * 3
