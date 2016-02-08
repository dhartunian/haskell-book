module Reverse where

getAwesome x = drop 9 x

rvrs :: String -> String
rvrs s = awesome ++ is ++ curry
  where awesome = getAwesome s
        curry = take 5 s
        is = drop 6 $ take 8 s

main :: IO ()
main = print $ rvrs "Curry is awesome"
