main =
  do putStrLn "Enter a character:"
     c <- getChar
     putStrLn ("You just typed " ++ [c])
