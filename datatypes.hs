module Datatypes where

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood _    = Blah

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhh"
  where cool a = a == "downright frosty yo"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs i =
  if i >= 0
     then i
  else -i

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

f' xs = w `x` 1
  where w = length xs

f'' (x:xs) = x

f''' z = fst z
