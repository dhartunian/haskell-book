module Jammin where
import Data.List

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry
    deriving (Eq, Show, Ord)

-- data JamJars =
--   Jam Fruit Int
--   deriving (Eq, Show)

data JamJars =
  Jam { fruit :: Fruit
      , jars :: Int}
  deriving (Eq, Show, Ord)


row1 = Jam Blackberry 10
row2 = Jam Apple 20
row3 = Jam Peach 30
row4 = Jam Plum 40
row5 = Jam Blackberry 3
row6 = Jam Apple 10
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = foldr ((+) . jars) 0

mostRow :: [JamJars] -> JamJars
mostRow xs = foldr biggerJar (head xs) (tail xs)
  where biggerJar a b =
          case compare (jars a) (jars b) of
            GT -> a
            LT -> b
            EQ -> a

compareKind (Jam k _) (Jam k' _) = compare k k'

sortJars :: [JamJars] -> [JamJars]
sortJars = sortBy compareKind

-- def took me a while to realize that sorting was required for groupBy to work properly
groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\a b -> (compareKind a b) == EQ) . sortJars
