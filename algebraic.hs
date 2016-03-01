module Algebraic where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1. Doggies is a type constructor
-- 2. Doggies is of kind * -> *
-- 3. Doggies String is of kind *
-- 4. Husky 10 :: Doggies Int
-- 5. Husky (10 :: Integer) :: Doggies Integer
-- 6. Mastiff "S" :: Doggies String
-- 7. DogueDeBordeaux is a data constructor and a type constructor
-- 8. DogueDeBordeaux "doggie" :: DogueDeBordeaux String


data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline =
    PapuAir
  | CatapultsR'us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Integer
    deriving (Eq, Show)

myCar    = Car Mini (Price 14000) :: Vehicle
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir 1000

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
