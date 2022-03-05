-- DemoStateTMonad.hs
-- John O'Donnell

{- An example showing how to use the StateT monad.  Before reading
this example, study DemoStateMonad, which is similar to this one.  The
computations and algorithms here are the same as in DemoStateMonad,
but here the StateT monad is used to provide IO as an inner monad, so
that I/O can be performed while runnning the program. -}

module DemoStateTMonad where
import Control.Monad.State

{- The main program executes the computation on two different initial
states. -}

main :: IO ()
main =
  do putStrLn "DemoStateTMonad"
     run 100  -- expected result = 6*(100+1) + 1 = 607
     run 5    -- expected result = 6*(5+1) + 1 = 37

{- Similar to run in DemoStateMonad.  However, runStateT compute n is
a computation, not a data value, so it needs to be performed using the
<- operator. -}

run :: Int -> IO ()
run n = 
  do putStrLn ("\nRunning with initial state " ++ show n)
     (a,s) <- runStateT compute n
     putStrLn ("Return value = " ++ show a)
     putStrLn ("Final state = " ++ show s)

{- We can't do putStrLn in the compute operation, because compute is
in the StateT monad and putStrLn requires IO.  However, we can lift
putStrLn into the StateT s IO a monad.  Instead of writing "liftIO
(putStrLn xs)" repeatedly in compute, it's more concise to define
sputStrLn once. -}

sputStrLn :: String -> StateT a IO ()
sputStrLn xs = liftIO (putStrLn xs)

{- Another useful helper operation is printState, which uses get to
obtain the state and then prints it. -}

printState :: Show a => StateT a IO ()
printState =
  do s <- get
     sputStrLn ("Current state is " ++ show s)

{- The following definition is similar to compute in DemoStateMonad,
but now we can perform I/O in the computation.  You can't do that in
DemoStateMonad; if you try, there would be a type error. -}

compute :: StateT Int IO ()
compute=
  do sputStrLn "Starting compute"
     printState
     sputStrLn "About to increment the state"
     incState
     let factor = 6
     sputStrLn ("About to multiply the state by " ++ show factor)
     mulState factor
     printState
     sputStrLn "About to increment the state"
     incState
     printState
     sputStrLn "Compute is finished"
     return ()

{- incState is almost identical to incState in DemoStateMonad; the
only difference is the type signature.  Here, incState runs in the
StateT monad with state type Int and inner monad IO.  The actual code
is the same -- that's made possible by the flexibility of type
classes.  -}

incState :: StateT Int IO ()
incState =
  do s <- get
     put (s+1)
     return ()

{- Again, mulState is the same except for the type signature. -}

mulState :: Int -> StateT Int IO ()
mulState n =
  do s <- get
     put (s*n)
     return ()
