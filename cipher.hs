module Cipher where
import Data.Char

genRotate :: Int -> Int -> (Int -> Int -> Int)
rotateChar :: Int -> Char -> Char
caesar :: Int -> String -> String
uncaesar :: Int -> String -> String

genRotate startId numLetters =
  \n i -> mod (n + (i - startId)) numLetters + startId

rotateChar n c
  | elem c ['a'..'z'] = chr . (rotateLower n) . ord $ c
  | elem c ['A'..'Z'] = chr . (rotateUpper n) . ord $ c
  | otherwise = c
    where rotateLower = genRotate (ord 'a') 26
          rotateUpper = genRotate (ord 'A') 26

caesar = map . rotateChar

uncaesar = map . rotateChar . negate
