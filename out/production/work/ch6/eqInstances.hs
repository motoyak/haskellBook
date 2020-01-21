module EqInstances where

data TisAnInteger =
   TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'


data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v w) (Two v' w') =
       v == v'
    && w == w'


data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString v) (TisAString v') = v == v'
  (==) _ _ = False


data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair v' w') =
       v == v'
    && w == w'


data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) =>  Eq (Tuple a b) where
  (==) (Tuple v w) (Tuple v' w') =
       v == v'
    && w == w'


data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _ _ = False


data EitherOr a b =
    Hello a
  | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (GoodBye v) (GoodBye v') = v == v'
  (==) _ _ = False
