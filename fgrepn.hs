-- Name: Wang Xi
-- Student ID: 71875994

import System.Environment
import Data.List

main = do args <- getArgs
          cs <- getContents
          putStr $ fgrepn (head args) cs

fgrepn :: String -> String -> String
fgrepn pattern cs = unlines $ filter match $ numbering cs
  where 
    match :: String -> Bool
    match line = any prefixp $ tails line

    prefixp :: String -> Bool
    prefixp line = pattern `isPrefixOf` line

    numbering :: String -> [String]
    numbering cs = map format $ zipLineNumber $ lines cs

    zipLineNumber :: [String] -> [(Int, String)]
    zipLineNumber xs = zip [1..] xs

    format :: (Int, String) -> String
    format (n, line) = rjust 4 (show n) ++ " " ++ line

    rjust :: Int -> String -> String
    rjust width s = replicate (width - length s) '0' ++ s