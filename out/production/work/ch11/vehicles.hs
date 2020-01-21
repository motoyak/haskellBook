module Vehicles where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'US
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

newtype Size = Size (Integer, Integer) deriving (Eq, Show)

myCar :: Vehicle
myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size (100, 200))

isCar :: Vehicle -> Bool
isCar x = case x of
            Car _ _ -> True
            _ -> False

isPlane :: Vehicle -> Bool
isPlane x = case x of
              Plane _ _ -> True
              _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu x = case x of
              Car manu _ -> manu
              _ -> error "manufacturer is undefined"
