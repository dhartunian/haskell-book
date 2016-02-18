module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Unknown digit"

digits :: Int -> [Int]
digits n = go n []
  where go n' xs
          | n' < 10 = [n'] ++ xs
          | n' >= 10 = go (div n' 10) ([(mod n' 10)] ++ xs)
          | otherwise = error "bad"

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
