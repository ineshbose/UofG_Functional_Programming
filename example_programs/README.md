# Example Haskell Programs

The following programs illustrate Haskell programming techniques. A Haskell script file should have a name like Foo.hs (the file extension ".hs" stands for Haskell Script). The files below have ".txt" added to the end (for example, Foo.hs.txt) so that you can read the text in Moodle just by clicking on the file name. To run an example, save the file and remove the .txt from the filename. Then launch ghci and load the program by entering ":load Program.hs", then run the main function. Alternatively, compile the source using ghc and run the executable.

## Program constructs

- LetExamples.hs - some examples of let expressions
- WhereExamples.hs - some function definitions where equations use where clauses to provide local variables for the right hand sides.

## Using the I/O monad

- HelloWorld.hs - the classic Hello World program in Haskell.
- CalculatePut.hs - perform a simple calculation and print the result. Example of do in IO monad and putStrLn.
- GetLine.hs - read a line of text, terminated by a newline.
- GetChar.hs - read a single character and print it; illustrates use of characters and strings.
- GetCalculatePut.hs - read input from the user, convert it from text to a number, perform a simple calculation, and print the result. Example of getLine and read.
- Starman.hs - a simple game, illustrating text I/O, interaction, and list processing.

## Parsing

- ParseVars.hs - illustrates how to parse variable names, with two slightly different syntaxes.
- GetParseResult.hs - shows how to obtain the result of a parse and use it in an interpreter.

## Monads

- DemoStateMonad.hs - shows how to handle state with the basic State monad
- DemoStateTMonad.hs - similar to the above, but StateT is used to allow I/O to be performed in the stateful computation.
