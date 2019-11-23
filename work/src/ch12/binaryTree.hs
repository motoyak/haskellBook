module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z =
  case f z of
    Nothing -> Leaf
    Just (t, u, v) -> Node (unfold f t) u (unfold f v)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f m
      | m == n = Nothing
      | otherwise = Just (m+1, m, m+1)
