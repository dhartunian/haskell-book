module Recursion where

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

summer :: (Eq a, Num a) => a -> a
summer num = go num 0
  where go n sumsofar
          | n == 0 = sumsofar
          | otherwise = go (n-1) (sumsofar + n)

mymult :: (Integral a) => a -> a -> a
mymult m n = go m n m
  where go m' n' prod
          | n' == 1 = prod
          | otherwise = go m' (n' - 1) (prod + m')


data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom = go (abs num) (abs denom) 0
  where
    sign = (signum num) * (signum denom)
    go n d count
          | d == 0 = DividedByZero
          | n < d = Result $ fromIntegral $ sign * count
          | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
  | n <= 100 = mc91 $ mc91 $ n + 11
  | n > 100  = n - 10
  | otherwise = error "hmm"
