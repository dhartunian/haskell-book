module Cipher where
import Data.Char

rotate :: Int -> Int -> Int
rotateChar :: Int -> Char -> Char
caesar :: Int -> String -> String
uncaesar :: Int -> String -> String


rotate n i = mod (n + (i - startId)) numLetters + startId
  where startId = ord 'a'
        numLetters = 26

rotateChar n = chr . (rotate n) . ord

caesar = map . rotateChar

uncaesar = map . rotateChar . negate
