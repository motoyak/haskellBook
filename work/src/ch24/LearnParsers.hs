module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"


-- exercise 2
p123 :: String -> IO ()
p123 xs =
  let p = case xs of
            "1" -> string "1"
            "12" -> string "12"
            "123" -> string "123"
            _ -> stop
  in
    print $ parseString p mempty xs

p123' :: String -> IO ()
p123' xs =
  let p :: Parser String
      p = case xs of
            "1" -> string "1" >> stop
            "12" -> string "12" >> stop
            "123" -> string "123" >> stop
            _ -> stop
  in
    print $ parseString p mempty xs

-- exercise 3
p123c :: String -> IO ()
p123c xs =
  let p = case xs of
            "1" -> char '1'
            "12" -> char '1' >> char '2'
            "123" -> char '1' >> char '2' >> char '3'
            _ -> stop
  in
    print $ parseString p mempty xs
    

pNL s = putStrLn ('\n' : s)

main = do

  pNL "stop:"
  testParse stop
  
  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'
  
  pNL "oneTwo:"
  testParse oneTwo
  
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneEof':"
  print $ parseString (one>>eof) mempty "123"
-- exercise 1
  pNL "oneTwoEof':"
  print $ parseString (oneTwo>>eof) mempty "123"

  pNL "oneTwoThreeEof':"
  print $ parseString (oneTwo>>char '3' >>eof) mempty "123"
