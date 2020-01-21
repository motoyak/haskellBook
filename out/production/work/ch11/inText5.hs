module InText5 where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType =
    DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving Show

data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

-- data Automobile = Null
                -- | Car { make :: String
                      -- , model :: String
                      -- , year :: Integer }
                -- deriving (Eq, Show)

data Car = Car { make :: String
               , model :: String
               , year :: Integer }
               deriving (Eq, Show)

data Automobile = Null | Automobile Car deriving (Eq, Show)


data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both


quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)


quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Yes
quantFlip8 No   = No
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Yes
quantFlip9 No   = No
quantFlip9 Both = Both

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Yes
quantFlip10 No   = Both
quantFlip10 Both = No

quantFlip11 :: Quantum -> Quantum
quantFlip11 Yes  = Yes
quantFlip11 No   = Both
quantFlip11 Both = Both

quantFlip12 :: Quantum -> Quantum
quantFlip12 Yes  = Both
quantFlip12 No   = Both
quantFlip12 Both = Both

quantFlip13 :: Quantum -> Quantum
quantFlip13 Yes  = No
quantFlip13 No   = Yes
quantFlip13 Both = No

quantFlip14 :: Quantum -> Quantum
quantFlip14 Yes  = No
quantFlip14 No   = Yes
quantFlip14 Both = Both

quantFlip15 :: Quantum -> Quantum
quantFlip15 Yes  = No
quantFlip15 No   = No
quantFlip15 Both = Yes

quantFlip16 :: Quantum -> Quantum
quantFlip16 Yes  = No
quantFlip16 No   = No
quantFlip16 Both = No

quantFlip17 :: Quantum -> Quantum
quantFlip17 Yes  = No
quantFlip17 No   = No
quantFlip17 Both = Both

quantFlip18 :: Quantum -> Quantum
quantFlip18 Yes  = No
quantFlip18 No   = Both
quantFlip18 Both = Yes

quantFlip19 :: Quantum -> Quantum
quantFlip19 Yes  = No
quantFlip19 No   = Both
quantFlip19 Both = No

quantFlip20 :: Quantum -> Quantum
quantFlip20 Yes  = No
quantFlip20 No   = Both
quantFlip20 Both = Both

quantFlip21 :: Quantum -> Quantum
quantFlip21 Yes  = Both
quantFlip21 No   = Yes
quantFlip21 Both = No

quantFlip22 :: Quantum -> Quantum
quantFlip22 Yes  = Both
quantFlip22 No   = Yes
quantFlip22 Both = Both

quantFlip23 :: Quantum -> Quantum
quantFlip23 Yes  = Both
quantFlip23 No   = No
quantFlip23 Both = Yes

quantFlip24 :: Quantum -> Quantum
quantFlip24 Yes  = Both
quantFlip24 No   = No
quantFlip24 Both = No

quantFlip25 :: Quantum -> Quantum
quantFlip25 Yes  = Both
quantFlip25 No   = No
quantFlip25 Both = Both

quantFlip26 :: Quantum -> Quantum
quantFlip26 Yes  = Both
quantFlip26 No   = Both
quantFlip26 Both = Yes

quantFlip27 :: Quantum -> Quantum
quantFlip27 Yes  = Both
quantFlip27 No   = Both
quantFlip27 Both = No


convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = False


data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)


data Silly a b c d =
  MkSilly a b c d deriving Show

-- data EsResultFound a =
  -- EsResultFound
  -- { _version :: DocVersion
  -- , _source :: a
  -- } deriving (Eq, Show)

data List a = Nil | Cons a (List a)
