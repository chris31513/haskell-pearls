module Strings where

    import Data.Char

    quitaMayusculas :: String -> String
    quitaMayusculas xs = [y | y <- xs, isLower y]

    soloLetras :: String -> String
    soloLetras xs = [y | y <- xs, isLetter y]

    prefijo :: String -> String -> Bool
    prefijo "" _ = True
    prefijo (x:xs) (y:ys) = (x == y) && (prefijo xs ys)