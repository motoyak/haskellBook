module InText2 where

class Impish v where
  impossibleKind :: v -> v a

class AlsoImp v where
  nope :: v a -> v
