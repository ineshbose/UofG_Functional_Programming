-- DemoStateMonad.hs
-- John O'Donnell

{- An example showing how to use the State monad.  Also see the
DemoStateTMonad example, which is similar to this one.  Run the
program by loading it into ghci and entering main. -}

module DemoStateMonad where
import Control.Monad.State

{- The main program executes the computation on two different initial
states. -}

main :: IO ()
main =
  do putStrLn "DemoStateMonad"
     run 100  -- expected result = 6*(100+1) + 1 = 607
     run 5    -- expected result = 6*(5+1) + 1 = 37

{- The run computation uses runState to execute the computation
"compute", which is defined below.  The initial state is given to be
n, the Int argument.  After runState delivers the return value a and
the final state s, these are printed.  In this example, the state is
simply an Int; naturally in a real program, the state would be
something nontrivial, such as a record or algebraic data type. -}

run :: Int -> IO ()
run n =
  do putStrLn ("\nRunning with initial state " ++ show n)
     let (a,s) = runState compute n
     putStrLn ("Return value = " ++ show a)
     putStrLn ("Final state = " ++ show s)

{- A typical computation in the State monad would perform a variety of
lower level operations, such as incState and mulState. -}

compute :: State Int ()
compute=
  do incState     -- add 1 to the state
     mulState 6   -- multiply the state by 6
     incState     -- add 1 to the state
     return ()

{- incState is a low level operation that uses get to obtain the
state, adds 1 to it, and puts the result back into the state.  -}

incState :: State Int ()
incState =
  do s <- get
     put (s+1)
     return ()

{- mulState is another low level operation similar to incState, but
mulState is a function that takes an argument n::Int and multiplies
the state by that. -}

mulState :: Int -> State Int ()
mulState n =
  do s <- get
     put (s*n)
     return ()
