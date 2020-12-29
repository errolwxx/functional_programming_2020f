main = do cs <- getContents
          putStrLn ((firstNLines 5 cs) ++ "...\n" ++ (lastNlines 5 cs)) 

firstNLines n cs = unlines $ take n $ lines cs
lastNlines n cs = unlines $ takeLast n $ lines cs
takeLast n ss = reverse $ take n $ reverse ss