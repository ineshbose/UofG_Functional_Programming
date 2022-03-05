{- WhereExamples.hs Some simple definitions giving examples of where
clauses. -}

{- Here is a simple example of a function defined using a where
clause.  Try working out the value of f1 7, and check it with the
computer. -}

f1 x = a^2 - b
  where a = x+1
        b = 2*3

{- This is essentially the same, with only a difference in spacing.
The f2 deifnition requires an extra line but it keeps the equations
indented a few spaces less deeply. -}

f2 x = a^2 - b
  where
    a = x+1
    b = 2*3

{- The following definition has three scopes: (1) the entire body of
the function is a local scope where x is bound; (2) the let expression
defines a scope where a and b are bound; (3) the right hand side of
the equation defining b is an innermost scope where a is bound to
10*x.  Wherever a name is used in an expression, it's important to be
clear about where that name is bound.  In general, this will be the
innermost enclosing scope.  Thus, in the right hand side of the
equation for b, the variable a refers to a = 10*x, but in the let
expression (a,b), the variable b refers to a = 3. Try to predict the
result of this expression when g is applied to some number (e.g. g 5)
and then evaluate it with ghci. -}

g :: Int -> (Int,Int)
g x =
  let a = 3
      b = a+1
        where a = 10*x
  in (a,b)
