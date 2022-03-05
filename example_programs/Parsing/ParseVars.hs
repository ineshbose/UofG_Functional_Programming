{- ParseVars.hs.  Solution to unassessed exercises.  Note that there
is an import statement at the beginning of the module, whilch loads
the Parsec library (this is necessary with some versions of the
compiler, not with others).  The run function is defined at the bottom
of the file; you can copy this verbatim into your own programs. -}

import Text.ParserCombinators.Parsec

{- A good way to run the unit tests is to have two windows on the
screen, one an editor opened to this file, and the other a shell
running ghci.  Select the text for a unit test, copy it, and paste it
in the ghci window. -}

------------------------------------------------------------------------
{- Write a parser "simpleVar" that accepts a variable name that
consists only of letters and underscores (the _ character).  The
variable name must contain at least one character, and it may begin
with either a letter or underscore. -}

simpleVar :: Parser String
simpleVar =
  do xs <- many1 (letter <|> char '_')
     return xs

{- Unit tests
run simpleVar "x"
run simpleVar "_"
run simpleVar "xab_rq_b"
run simpleVar "_dog_2"
run simpleVar "x2"          -- doesn't consume the 2
run simpleVar "3a"          -- fails
run simpleVar "*?"          -- fails
-}

------------------------------------------------------------------------
{- Write a parser "betterVar" that accepts a better form of variable
name, which may contain letters, digits, underscores (the _
character), primes (the ' character).  The variable name must begin
with a letter.  Only one prime is allowed in a name, and if a prime
appears, it must be the last character in the name.  See the unit
tests below for some examples.  (Note that this syntax is not the same
as Haskell variables, which allow several primes.) -}

betterVar :: Parser String
betterVar =
  do x <- letter
     xs <- many (letter <|> digit <|> char '_')
     prime <- string "'" <|> string ""
     return (x:xs ++ prime)

{- Unit tests
run betterVar "abcd"
run betterVar "_abcd"
run betterVar "a3_bc_d"
run betterVar "a3_bc_d'"
run betterVar "a3_b'xyz'"    -- ok but xyz isn't part of name
run betterVar "3x"           -- fails: must start with letter
-}

------------------------------------------------------------------------

noun, verb, whitespace, sentence :: Parser String

{- We need to be careful with nouns, because two of them --- mouse and
moose --- share some opening letters.  For this reason, we parse { try
(string "mouse") } instead of simply { string "mouse" }.  By putting
it in a try, an attempt to parse "mouse" when the input is "moose"
will fail, and the opening "mo" will not be consumed; this allows {
string "moose" } to succeed.  If we didn't have the try, the attempt
to parse "mouse" would fail but it would consume the opening "mo", so
then the attempt to parse "moose" would also fail. -}

noun =
  string "cat" <|> string "dog"
     <|> try (string "mouse") <|> string "moose"

verb = string "chases" <|> string "fears"

whitespace = many1 (char ' ')

sentence =
  do subj <- noun
     whitespace
     vrb <- verb
     whitespace
     obj <- noun
     return (subj ++ "/" ++ vrb ++ "/" ++ obj)

{- Unit tests
run sentence "cat chases dog"
run sentence "mouse fears cat"
run sentence "moose chases mouse"
run sentence "dog fears tiger"     -- fails at tiger
run sentence "cat licks fur"       -- fails at licks
run sentence "lion is big"         -- fails at lion (the second o)
-}
    

------------------------------------------------------------------------
{- The run function provides an easy interface for testing a
parser. -}

run :: Show a => Parser a -> String -> IO ()
run p input
  = case parse p "" input of
      Left err ->
        do putStr "parse error at "
           print err
      Right x -> print x
