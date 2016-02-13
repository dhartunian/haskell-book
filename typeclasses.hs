module MyTypeclasses where
import Data.List

data TisAnInteger =
  TisAn Integer deriving Show

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers =
  Two Integer Integer deriving Show

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = (a == a') && (b == b')

data StringOrInt =
    TisAnInt Int
  | TisAString String deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a1' a2') = (a1 == a1') && (a2 == a2')

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'


data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
               else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "I" "Love" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

sigmund' :: Num a => a -> Int
sigmund' x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = let b = f a
              in foldr (+) b (replicate (fromIntegral i) b)
