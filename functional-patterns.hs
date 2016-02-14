module FnP where

addOneIfOdd :: Integer -> Integer
addOneIfOdd n = case odd n of
  True -> f' n
  False -> n
  where f' = \z -> z + 1

addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip :: (b -> a -> z) -> a -> b -> z
mflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

f'' :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f'' (a,_,c) (d,_,f) = ((a,d), (c,f))

functionC :: Ord(z) => z -> z -> z
functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral z => z -> z
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums :: (Num z, Ord z) => z -> Integer
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank ::
  (Employee -> Employee -> Ordering)
  -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

compare' :: Employee -> Employee -> Ordering
compare' Coder Coder = EQ
compare' Coder _     = GT
compare' _     Coder = LT
compare' e     e'    = compare e e'

dodgy :: Num(z) => z -> z -> z
dodgy x y = x + y * 10

oneIsOne :: Num(z) => z -> z
oneIsOne = dodgy 1

oneIsTwo :: Num(z) => z -> z
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59  = 'D'
  | y < 0.59  = 'F'
  | otherwise = 'F'
  where y = x / 100

pal :: (Eq a) => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False


numbers :: (Num a, Ord a, Num b) => a -> b
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
  | otherwise = 0



tensDigit :: Integral a => a -> a
tensDigit x = d
  where (z,_) = divMod x 10
        (_,d) = divMod z 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' a1 a2 b1
  | b1 == True = a1
  | b1 == False = a2
  | otherwise = a2

g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)


