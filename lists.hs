module MyLists where
import Data.Char

myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo a b = go a b []
  where go a' b' xs
          | (fromEnum a') > (fromEnum b') = xs
          | otherwise = go (succ a') b' (xs ++ [a'])

separateBySpaces :: String -> [String]
separateBySpaces s = go s []
  where go [] xs = xs
        go s'  xs = go rest $ xs ++ [nextWord]
          where nextWord  = (takeWhile notSpace s')
                rest      = drop 1 (dropWhile notSpace s')
                notSpace  = (/=)' '

ff :: Num a => a -> a
ff x = x

-- NF   --  [1,2,3,4,5]
-- WHNF --  1 : 2 : 3 : 4 : _
-- WHNF --  enumFromTo 1 10
-- WHNF --  length [1, 2, 3, 4, 5]
-- WHNF --  sum (enumFromTo 1 10)
-- Nei  --  ['a'..'m'] ++ ['n'..'z']
-- WHNF --  (_, 'b')

myFilter :: String -> [String]
myFilter = filter (not . flip elem ["the", "a", "an"]) . words

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ (zip xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = [f x y] ++ (zipWith' f xs ys)


zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

-- chapter exercises

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = [toUpper x] ++ xs

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = [toUpper x] ++ capitalizeAll xs

getFirstCapital :: String -> Char
getFirstCapital = toUpper . head


-- standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f(x) || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
--myElem a [] = False
--myElem a (x:xs) = a == x || myElem a xs
myElem a = myAny (==a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f(x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain =  squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where go f [] x = x
        go f (x:xs) x' = case f x' x of
          GT -> go f xs x'
          LT -> go f xs x
          EQ -> go f xs x'

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where go f [] x = x
        go f (x:xs) x' = case f x' x of
          GT -> go f xs x
          LT -> go f xs x'
          EQ -> go f xs x

mymax :: (Ord a) => [a] -> a
mymax = myMaximumBy compare

mymin :: (Ord a) => [a] -> a
mymin = myMinimumBy compare
