main= do cs <- getContents
         putStr $ reverseLines cs

reverseLines cs = unlines $ reverse $ lines cs 