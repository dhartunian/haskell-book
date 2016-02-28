module FoldingLists where
import Data.Time
import Data.List

left = foldl (+) 0 (1 : 2 : 3 : [])
-- ((0 + 1) + 2) + 3
right = foldr (+) 0 (1 : 2 : 3 : [])
-- (1 + (2 + (3 + 0)))

-- foldl (flip (*)) 1 [1..3]
-- (3 * (2 * (1 * 1)))

a = foldr (++) [] ["woot", "WOOT", "woot"]
b = foldr max [] ["fear", "is", "the", "little", "death"]
c = foldr (&&) True [False, True]
d = foldr (||) True [False, True]
e = foldl (flip $ (++) . show) "" [1..5]
f = foldr (flip const) 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]

data DatabaseItem =
  DbString String
  | DbNumber Integer
  | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello World"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate time) list = [time] ++ list
        f _ list = list

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber i) list = [i] ++ list
        f _ list = list

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr max (head dates) (tail dates)
  where dates = filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = dbSum / dbLength
  where dbSum    = fromIntegral $ sumDb xs
        dbLength = fromIntegral $ length $ filterDbNumber xs

myFibs :: Integer -> Integer
myFibs 0 = 1
myFibs 1 = 1
myFibs n = (myFibs (n - 1)) + (myFibs (n - 2))

-- 1 : [1 + 1, 1 + 2, 2 + 3]
fibs = 1 : scanl (+) 1 fibs
-- take 3 fibs
-- 1 : scanl (+) 1 fibs
-- 1 : scanl (+) 1 (1 : scanl (+) 1 fibs)
-- 1 : scanl (+) 1 (1 : scanl (+) 1 (1 : scanl (+) 1 fibs))

--           v-------vv-------v
-- [1, (1 + 1), (1 + 2), (2 + 3), (3 + 5)]
--          \----/   \----/

--
-- 1 :             scanl (+) 1       fibs
-- 1 :             scanl (+) 1       (1 : scanl (+) 1 fibs)
-- 1 : 1 :         scanl (+) (+ 1 1) (scanl (+) 1 fibs)
-- 1 : 1 :         scanl (+) 2       (scanl (+) 1 (1 : scanl (+) 1 fibs))
-- 1 : 1 :         scanl (+) 2       (1 : scanl (+) (+ 1 1) (scanl (+) 1 fibs))
-- 1 : 1 : 2       scanl (+) (+ 2 1) (scanl (+) 2 (scanl (+) 1 fibs))
-- 1 : 1 : 2       scanl (+) 3       (scanl (+) 2 (scanl (+) 1 fibs))
-- 1 : 1 : 2       scanl (+) 3       (scanl (+) 2 (1 : scanl (+) (+ 1 1) ()))

-- 1 : 1 : 2 :     scanl (+) (+ 2 1) (scanl (+) (+ 1 1) fibs)
-- 1 : 1 : 2 :     scanl (+) 3       (2 : scanl (+) (+ 2 1) (scanl (+) 1 fibs))
-- 1 : 1 : 2 : 3 : scanl (+) (+ 3 2) (scanl (+) 3 (scanl (+) 1 fibs))
-- 1 : 1 : 2 : 3 : scanl (+) 5       (3 : scanl (+) (+ 3 ))

-- fibs = [1 : 1 : 2 : 3]


myFactorial :: Integer -> Integer
myFactorial 0 = 1
myFactorial n = n * (myFactorial (n - 1))

-- assume we can generate factorial of n
-- how do we generate factorial of n+1?
-- fact 0     = 1
-- fact n     = n * (fact (n - 1))

factorial = scanl (*) 1 [2..]

--      v        vvvvv        vvvvvvvvv
-- [1, (1 * 2), (1 * 2 * 3), (1 * 2 * 3 * 4)]
--

-- (+) 0 [1..10]
-- (0 + (1 + (2 + (3 + ))))

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x $ myFoldr f b xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs


stops = "pbtdkg"
vowels = "aeiou"

combos = [[s,v,s'] | s <- stops, v <- vowels, s' <- stops, s == 'p']

nouns = ["house", "dog", "tree", "cat"]
verbs = ["hug", "bite", "run", "grow", "whistle", "sleep"]

combos' = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns]

avgLengthOfWord x = fromIntegral (sum (map length (words x))) / (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem = myAny . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr z []
  where z a b = if f a then [a] ++ b
                else b

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- need to use foldl instead of foldr to ensure L->R ordering
-- myMaximumBy (\_ _ -> GT) [1..10] should == 1
-- myMaximumBy (\_ _ -> LT) [1..10] should == 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = foldl takeGT x xs
  where takeGT a b = case (f a b) of
          GT -> a
          otherwise -> b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) = foldl takeLT x xs
  where takeLT a b = case (f a b) of
          LT -> a
          otherwise -> b
