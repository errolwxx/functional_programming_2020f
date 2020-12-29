-- Name: Wang Xi
-- Student ID: 71875994
-- CNS: t18599xw

import System.Environment
import Data.List

main = do args <- getArgs 
          let year = read $ args !! 0
          let month = read $ args !! 1
          let day = read $ args !! 2
          putStrLn $ yearStr year month (day + 10)
          putStrLn $ yearStr year month (day + 100)
          putStrLn $ yearStr year month (day + 1000)
          putStrLn $ yearStr year month (day + 10000)
        --   putStrLn $ yearStr year month day

leap :: Int -> Bool
leap y | y `mod` 400 == 0 = True
       | y `mod` 100 == 0 = False
       | y `mod` 4 == 0 = True
       | otherwise = False

monthCheck :: Int -> Int -> Int
monthCheck _ 1 = 31
monthCheck y 2 = if leap y then 29 else 28
monthCheck _ 3 = 31
monthCheck _ 4 = 30
monthCheck _ 5 = 31
monthCheck _ 6 = 30
monthCheck _ 7 = 31
monthCheck _ 8 = 31
monthCheck _ 9 = 30
monthCheck _ 10 = 31
monthCheck _ 11 = 30
monthCheck _ 12 = 31
monthCheck y m = 0

yearStr :: Int -> Int -> Int -> String
yearStr y m d | (map sum $ tails $ map (monthCheck y) [1..12]) !! (m-1) >= d = 
                let
                    -- remList = map sum $ tails $ map (monthCheck y) [1..12]
                    elpList = reverse $ map sum $ tails $ reverse $ map (monthCheck y) [1..11]
                    changedMon = head $ elemIndices (head $ reverse $ filter (<=d) elpList) elpList
                    changedDay = subtract (head $ reverse $ filter (<d) (0:elpList)) d
                in show y ++ "/" ++ show (changedMon+1) ++ "/" ++ show changedDay
              | otherwise = 
                let
                    rem = (map sum $ tails $ map (monthCheck y) [1..12]) !! (m-1)
                in yearStr (y+1) 1 (d-rem)
