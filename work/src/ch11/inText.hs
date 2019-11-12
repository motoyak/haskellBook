module InText where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)


data Example0 = Example0 deriving (Eq, Show)
data Example1 = Example1 Int deriving (Eq, Show)
data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

data Example = MakeExample deriving Show

data Example' = MakeExample' Int deriving Show

data Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42


data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

type TwoQs' = (QuantumBool, QuantumBool)

data Person' =
  MkPerson' String Int
  deriving (Eq, Show)

jm' = MkPerson' "julie" 108
ca' = MkPerson' "chris" 16

namae' :: Person' -> String
namae' (MkPerson' s _) = s


data Person =
  Person { name :: String
         ,  age :: Int }
         deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show

--data BookType = FictionBook Fiction
              -- | NonfictionBook Nonfiction
              -- deriving Show

data Fiction' = FictionData deriving Show
data Nonfiction' = NonfictionData deriving Show

data BookType' = FictionBook' Fiction'
              | NonfictionBook' Nonfiction'
              deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr


type Gardener = String

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show
