-- GetParseResult.hs
-- John O'Donnell

{- This program illustrates how you can obtain the result of a parser,
and use it in a program.  To run it, load the program into ghci and
enter main, which will execute several test cases. -}

module Main where
import Text.ParserCombinators.Parsec

{- This is the standard run computation, which runs a parser and
prints the result.  It's good for testing your parser, but doesn't
provide the result in a form that the rest of the program can use. -}

run :: Show a => Parser a -> String -> IO ()
run p input
  = case (parse p "" input) of
      Left err ->
        do putStr "parse error at "
           print err
      Right x -> print x

{- Since a parser may succeed with a result, or fail with an error
message, we define ParseResult to represent the result.  This is
similar to using a Maybe type, but with Maybe there is no information
attached to Nothing.  Here, we want to return the error message. -}

data ParseResult a
  = GoodParse a
  | BadParse String

{- getParseResult takes a parser and a string.  It attempts to parse
the string, and if it succeeds it returns Just r, where r is a value
of the type the parser produces (e.g. an abstract syntax tree).  If
the parse fails, an error message is returned. -}

getParseResult :: Parser a -> String -> ParseResult a
getParseResult p xs =
  case parse p "" xs of
    Left err -> BadParse (show err)
    Right x -> GoodParse x

{- To illustrate how to use getParseResult, we define a dummy abstract
syntax tree, called Thing.  It has several alternatives, including a
string of letters and an Int. -}

data Thing
  = LetterThing String
  | NumberThing Int
  deriving (Read, Show)

{- The parser for Thing tries to parse a string of letters;
alternatively a string of digits. -}

parseThing = parseLetter <|> parseNumber

{- To parse a string of letters, the parser matches (many1 letter),
i.e. one or more letters.  If it succeeds, the string xs of letters is
put into a Thing by applying the constructor LetterThing to xs. If the
input doesn't begin with one or more letters, parseLetter simply
fails. -}

parseLetter :: Parser Thing
parseLetter =
  do xs <- many1 letter
     return (LetterThing xs)

{- parseNumber is similar to parseLetter.  If it succeeds in finding
one or more digits, it converts the string xs to an Int and puts it
into a Thing using the NumberThing constructor. -}

parseNumber :: Parser Thing
parseNumber =
  do xs <- many1 digit
     return (NumberThing (read xs :: Int))

{- The interpreter takes a string as input, parses it with parseThing,
uses getParseResult to obtain the result of the parse, and then does
some computation with the result.  Since the parse might fail,
getParseResult encapsulates the result in a Maybe, and the interpreter
uses a case to handle a successful parse or to handle a failure. -}

interpreter :: String -> IO ()
interpreter xs =
  do putStrLn ("\nStarting interpreter with input " ++ xs)
     let r = getParseResult parseThing xs
     case r of
       BadParse msg ->
         do putStrLn "Parse failed"
            putStrLn msg
            putStrLn "Quitting"
       GoodParse thing ->
         do putStrLn ("Result of parse = " ++ show thing)

{- The main program runs the interpreter separately on several examples. -}

main =
  do interpreter "zebra"
     interpreter "978"
     interpreter "*$>"

{- Unit tests
run parseThing "elephant"
run parseThing "234"
run parseThing "*?#"
-}

