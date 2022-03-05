{- LetExamples.hs Load this file in ghci.  First try to predict the
result of each expression, and then evaluate it with ghci.  For
example, enter example1 at the prompt and compare the computer's
result with your prediction. -}

example1 :: Int
example1 =
  2 * (let x = 2+2 in x*3) + 1

{- A short let expression can be done in a single line, as examples 1
and 2 show. -}

example2 :: (Int,Int)
example2 =
  let x = 2*3 in (x-1, x+1)

{- Notice the indentation in example3.  Each equation begins in the
same column, and the "in" keyword is lined up vertically below the
"let" keyword. -}

example3 :: Int
example3 =
  let x = y+5
      y = 7
  in x^2

{- Recommendation: use either the "entire let expression in one line"
style, as in examples 1 and 2, or the "standard let indentation style"
as in example 3. -}
