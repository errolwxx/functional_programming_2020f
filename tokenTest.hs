import Token

main = do cs <- getContents
          putStr $ unlines $ map (unwords . (map show) . tokens) $ lines cs