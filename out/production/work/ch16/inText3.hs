module InText3 where

data FixMePls =
    FixMe
  | Pls
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap = error "dummy"
