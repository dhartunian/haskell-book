module Print3 where

myGreeting :: String
myGreeting = (++ )"hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = (++) hello ((++) " " world)

exclaim :: String -> String
exclaim x = x ++ "!"

getY x = x !! 4

getAwesome x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs s = awesome ++ is ++ curry
  where awesome = getAwesome s
        curry = take 5 s
        is = drop 6 $ take 8 s
