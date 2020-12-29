main = do putStrLn "Input a name?"
          name <- getLine
          putStrLn ("Hello, " ++ name ++ " ")  