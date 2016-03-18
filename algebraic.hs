{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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

data Example2 a b = Example2 a b deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, Eq, TooMany)

newtype SpecialTooMany = SpecialTooMany (Int, String) deriving (Show, Eq)

instance TooMany SpecialTooMany where
  tooMany (SpecialTooMany (n,s)) = tooMany n

newtype TooMany3 = TooMany3 (Int, Int) deriving (Show, Eq)

instance TooMany TooMany3 where
  tooMany (TooMany3 (n1, n2)) = tooMany (n1 + n2)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, a') = (tooMany a) && (tooMany a')

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType =
  FictionBook Fiction
  | NonfictionBook Nonfiction
    deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- data FlowerType = Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--     deriving Show

type Gardener = String

data Garden =
  Gardenia Gardener
  | Rose Gardener
  | Lilac Gardener
  | Daisy Gardener
    deriving Show


data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
    deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

data OperatingSystem = Linux | OpenBSD | Mac | Windows deriving (Eq, Show)
data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [Linux, OpenBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  concat $ map (\os -> map (\lang -> Programmer { lang = lang, os = os }) allLanguages) allOperatingSystems


data Quantum = A | B | C deriving (Eq, Show)

convert :: Quantum -> Bool
convert A = True
convert B = True
convert C = True

convert2 :: Quantum -> Bool
convert2 A = True
convert2 B = True
convert2 C = False

convert3 :: Quantum -> Bool
convert3 A = True
convert3 B = False
convert3 C = True

-- 2^3 possible functions since we have a choice of 2 values for each of 3 possible inputs

data Quad = One | Two | Three | Four deriving (Eq, Show)

--1. 8 possible values
--2. 16 possible values
--3. 4^4 possible values
--4. 8 possible values
--5. 16 possible values
--6. 65536 possible values
