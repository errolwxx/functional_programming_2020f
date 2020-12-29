main = do cs <- getContents
          putStr $ lastNlines 10 cs

lastNlines n cs = unlines $ takeLast n $ lines cs

takeLast n ss = reverse $ take n $ reverse ss