{- This program illustrates how to read some text input, convert it to
a number, perform a simple calculation, and print the result. -}

main =
  do putStrLn "Enter a number"
     xs <- getLine
     let n = 3 * (read xs :: Int)
     putStrLn (show n)
