module ExFixerUpper where



ex1 = const <$> Just "Hello" <*> Just "World"

ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "tinerness" <*> Just [1,2,3]
