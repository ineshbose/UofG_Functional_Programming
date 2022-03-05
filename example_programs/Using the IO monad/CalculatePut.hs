{- Demonstration of outputting the result of a calculation, using
putStrLn in the IO monad.  To run this program, (1) launch ghci, (2)
load it, (3) enter main. -}

main =
  do putStrLn "About to do some difficult arithmetic..."
     let x = 2+2
     putStrLn ("The answer is " ++ show x)
