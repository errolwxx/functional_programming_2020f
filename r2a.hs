-- Name: Wang Xi
-- Student ID: 71875994
-- CNS: t18599xw

import System.Environment
import Data.List

main = do args <- getArgs
          print $ r2a $ head args

r2a :: String -> Int
r2a [] = 0
r2a n = let (rome, m) = dropEnd n
        in rome + r2a (drop (length m) n)

dropEnd :: String -> (Int, String)
dropEnd n = let roman = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
                decimal = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
            in head (dropPat (\(_, a) -> not (a `isPrefixOf` n)) (zip decimal roman))
  where 
    dropPat :: (a -> Bool) -> [a] -> [a]
    dropPat con xs@(x:xt) | con x = dropPat con xt
                          | otherwise = xs
