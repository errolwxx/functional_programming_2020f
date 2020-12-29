main = do putStrLn "Input a name?"
          name1 <- getLine
          putStrLn "Input another name?"
          name2 <- getLine
          putStrLn (name1 ++ " and " ++ name2 ++ " are friends.") 