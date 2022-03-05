-- Starman: an interactive text version of the hangman game.
-- John O'Donnell, October 2010

module Main where

------------------------------------------------------------------------
-- Main program

{- To play a game, the starman function is applied to the target word
and the number of stars the player has to work with.  For example, you
could launch the game by loading it in ghci, and entering: starman
"elephant" 4 -}

starman :: String -> Int -> IO ()
starman word n = move word ['_' | x <- word] n

------------------------------------------------------------------------
-- Checking a guess

{- Check a character c guessed by the player.  If the guess is correct
(i.e. c is in word) then the display should be updated with the
character; otherwise the display is unchanged. -}

check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word,
     [if x==c then c else y | (x,y) <- zip word display])

------------------------------------------------------------------------
-- A move in the game

{- Perform one move.  If there are no remaining stars (n==0) the
player has lost, and if the player has guessed the full word
(i.e. word==display) then the player has won.  If the game hasn't
ended with a win or loss, use try to carry out the next guess. -}

move :: String -> String -> Int -> IO ()
move word display n =
  do if n==0
       then putStrLn "You lose"
       else if word==display
              then putStrLn "You win!"
              else try word display n

{- Obtain a guess from the player, check it, and update the game
state. -}

try word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getChar
     putStrLn ""
     let (correct, display') = check word display q
     let n' = if correct then n else n-1
     move word display' n'
