module MyTypes where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)

--h :: (Num a, Num b) => a -> b -> b
--h a b = b + b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = a


co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g
-- also: co f g x = f $ g x
-- also: co f g x = f (g x)

a :: (a -> c) -> a -> a
a f g = g

a' :: (a -> b) -> a -> b
a' f zz = f zz

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q


data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f
