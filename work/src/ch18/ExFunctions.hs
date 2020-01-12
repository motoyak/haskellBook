module ExFunctions where

import Test.Hspec

-- 1
j :: Monad m => m (m a) -> m a
j x = x >>= id

test1 :: IO ()
test1 = hspec $ do
  describe "Excercise 1" $ do
    it "j [[1,2], [], [3]] is [1,2,3]" $ do
      j [[1,2], [], [3]] `shouldBe` [1,2,3]
    it "j (Just (Just 1)) is Just 1" $ do
      j (Just (Just 1)) `shouldBe` Just 1
    it "j (Just Nothing) is Nothing" $ do
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "j Nothing is Nothing" $ do
      j Nothing `shouldBe` (Nothing :: Maybe Int)


-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= return . f

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = do
  x <- a
  y <- b
  return $ f x y
  
l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f a b =
  a >>=
    \x ->
     b >>=
      \y ->
       return $ f x y

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a x' k = do
  x <- x'
  f <- k
  return $ f x
  
a' :: Monad m => m a -> m (a -> b) -> m b
a' x' k =
  x' >>=
   \x ->
    k >>=
     \f ->
      return $ f x

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] k = return []
meh (x:xs) k = do
  h <- k x
  t <- meh xs k
  return $ h : t
  
meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] k = return []
meh' (x:xs) k =
  k x >>=
   \h ->
    meh xs k >>=
     \t ->
      return $ h : t

-- 6
flipType :: Monad m => [m a] -> m [a]
flipType [] = return []
flipType xs = meh xs (\x -> j . return $ x)

