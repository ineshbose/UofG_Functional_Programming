# Week 4: When Programs Get Bigger

## Program Structure

How to organise larger programs

### Welcome to Week 4

```
0:08
WIM: Hi, and welcome to week four of our Haskell course. And this week we have two topics. First, we will look at some more advanced patterns of computation based on using functions as values. As an example we will look at parsing. Parsing is the process of analyzing text of a natural language or programming language according to the rules of the formal grammar. You will learn to use parsec. This is a Haskell library built out of what is called monadic parser combinators. And this might sound very complicated, but it’s really just a way of combining tiny building blocks into very powerful parts. The other topic of week four is program structure.

0:49
When you start building more complicated programs, then you have to start thinking a bit more carefully about how you structure your program. We will first look at a program scope. And scoping is the real key feature that you use to create modular programs. And then we will look at conditionals. You have already learned the if-then-else conditional, but sometimes that’s not really the best way to structure your program with conditional expressions, and therefore Haskell provides you with a large number of alternatives, and we will visit all of them. Hope you like this week and see you soon.
```

**Until now, most of our Haskell programs have been fairly small. This week, we will focus on techniques for developing larger, non-trivial programs.**

We will explore various ways to structure programs in Haskell, ideally to make the code more readable. We present an example text parser that uses the Parsec library. Finally we will advocate the use of automated testing with the QuickCheck tool.

### Keep Your Programs Tidy

**Scoping is an important way to keep your programs tidy. It involves limiting the region of the program in which names ‘exist’ and can be used.**

In Haskell, a `let` expression provides local scope. A `let` expression has a series of equations defining variable values and a final expression (after the `in` keyword) that computes a value with those variables in scope.

Here is an example:

```hs
let x = 2
in x*x
```

What will this expression evaluate to?

Multiple variables can be defined in a single let

```hs
let x = 2
    y = 3
in x+y
```

What will this expression evaluate to?

Note that the variable names line up underneath one another This is good formatting practice, but is also needed for Haskell to interpret the code correctly. Like Python, _whitespace is important_ in Haskell, certainly in `let` expressions anyway!

Sometimes in a `let`, one of the variables might depend on another — in the function below, `gallons` depends on `milespergallon`:

```hs
journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre =
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles/milespergallon
 in (gallons*litrespergallon*fuelcostperlitre)
```

Here’s a geometric example:

```hs
let diameter = 2*radius
    circumference = pi*diameter
in (diameter, circumference)
```

By the way, `pi` is a defined constant in the Haskell Prelude.

#### Where clauses

There is another syntax for introducing local variables, called the `where` clause. As we have seen, Haskell is the Swiss army knife of programming languages: there are lots of ways of doing things.

The `where` keyword, inside an equation, provides definitions for variables that are used in the equation.

Here are a couple of examples

```hs
squareplusone :: Int -> Int
squareplusone x = xsquared + 1
 where xsquared = x*x
```

Note that `where` must be indented more than the start of the enclosing equation.

Like `let`, we can have multiple variables inside a `where` clause. Look at this temperature conversion function:

```hs
cel2fahr :: Float -> Float
cel2fahr x = (x*scalingfactor) + freezingpoint
 where scalingfactor = 9.0/5.0
       freezingpoint = 32
```

Again, notice that the variables defined in the `where` clause all line up underneath each other.

You might think `let` and `where` are very similar!

- Both introduce a local scope.
- Both allow any number of equations to be written.
- Both allow the equations to be written in any order, and variables defined in any equation can be used (“are in scope”) in the other equations.

However there are some important _differences_:

- `let` expressions are expressions; let can be used anywhere an expression is allowed.
- `where` clauses are not expressions; they can be used only to provide some local variables for a top level equation.

### Guards, Guards

**Haskell provides a notation for defining functions based on predicate values.**

```hs
f x
  | predicate1 = expression1
  | predicate2 = expression2
  | predicate3 = expression3
```

For instance, the _absolute_ value of a number is its magnitude, i.e. ignoring its sign. You could define a function to calculate the absolute value with an if/then/else conditional

```hs
absolute x = if (x<0) then (-x) else x
```

or with guards

```hs
absolute x
  | x<0 = -x
  | otherwise = x
```

Notice how there is no equals sign on the first line of the function definition — but there is an equals sign after each guard.

The `otherwise` guard should always be last, it’s like the `default` case in a C-style `switch` statement.

Guards are easier to read than if/then/else if there are more than two conditional outcomes

For instance, think about scoring in the sport of Golf. For a single hole, a player takes a number of strokes. There is a ‘par’ score for the hole, which is the expected number of strokes.

```hs
holeScore :: Int -> Int -> String
holeScore strokes par
  | strokes < par = show (par-strokes) ++ " under par"
  | strokes == par = "level par"
  | strokes > par = show(strokes-par) ++ " over par"
```

How could we tidy this up? Maybe we could turn the final guard into `otherwise` and also refactor with a `where` clause.

```hs
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par
```

Notice that the `score` variable defined in the `where` clause is in scope for all three guards.

#### Case expressions

Recall from last week how we defined algebraic data types like binary trees. A value with an algebraic data type may have one of several different forms — such as a `Leaf` or a `Node`, in the case of `Tree` structures. Therefore to process such a value we need several segments of code, one for each possible form. The `case` expression examines the value, and chooses the corresponding clause. It’s like a guard, but it selects based on the form of the value, i.e. it does pattern matching.

Here is a sum data type for my pets.

```hs
data Pet = Cat | Dog | Fish
```

And here is how I greet my pets.

```hs
hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
```

Note that each pattern is followed by an arrow and then a value. Also note that each pattern is vertically aligned. Indentation really matters in Haskell!

OK, now suppose we want to make the data type a bit more sophisticated . Let’s add a `Parrot` with a `String` name.

```hs
data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name
```

Now the pattern includes a variable, which is associated with the concrete value for the Parrot’s name.

```hs
hello (Parrot "polly")
```

will return

```hs
"pretty polly"
```

In the same way as there is a catch-all case for guards (`otherwise`), we can have a catch-all pattern for a `case`. It’s the underscore character, `_` which means ‘don’t care’ or ‘match anything’

So we could redefine `hello` as:

```hs
hello :: Pet -> String
hello x =
  case x of
    Parrot name -> "pretty " ++ name
    _ -> "grunt"
```

How would you rewrite an `if` expression as a `case` expression? In fact, this is what happens in the core language. The `if` expression is just _syntactic sugar_ that is rewritten automatically. Can you think of any more examples of syntactic sugar in Haskell?

### Dealing with Uncertainty

```
0:06
JEREMY: Do you want to know something upsetting? Programs fail. Yes, computation often goes wrong. Haskell provides several mechanisms for handling computations that fail. The first– probably the simplest such mechanism– is called Maybe.

0:28
Let’s think about how a computation might fail. Imagine trying to process a list somehow. Say, take the first element of the list. And you are given an empty list. There isn’t any first element. Or imagine dividing two integers and the divisor is 0.

0:51
We need a way to capture whether or not such a computation has succeeded or whether it’s failed. This is the job of Maybe.

1:02
Let’s have a look at this data type. It’s defined in the Haskell prelude. The standard library for Haskell. So here I am in Google. I’m going to type in Haskell prelude and Maybe. Wait for the results. And the first hit is the Hackage site which is the official documentation for the Haskell platform. Let’s click on that. And here we are. Documentation for the prelude standard module. Let’s search in here for Maybe. Here we are. And we see that Maybe represents or encapsulates an optional value. So the value might either be Just a, for some a. Or it’s empty which is Nothing. The documentation goes on to say, using Maybe is a good way to deal with errors or exceptional cases.

2:00
So these are the type constructors. Either Nothing or Just a. We can look at the source as well. Let’s click here and have a look at the source for Maybe. And this is very familiar to us as an algebraic data type. Similar to the ones that we defined earlier on in the course. Maybe is either Nothing or Just a. For the type variable a. And again, we have these two type classes that Maybe derives. Eq for equality. And Ord for comparison. Right. Let’s have a play with Maybe. Imagine we want a function to find the maximum value in the list. Perhaps a list of integers. Let’s start off by defining the maxhelper routine.

2:57
So maxhelper is going to take an integer value which is the maximum we’ve seen so far. And an integer list which is the rest of the list we still have to process to look to the maximum value. And it’s going to return an int, which is the maximum value over the whole list. So maxhelper. If we’re given an interim maximum and there’s nothing else left to look at, the list just returns x. That is the maximum. Maxhelper with an interim value x and a nonempty input list is going to call itself recursively.

3:38
And the interim maximum is either going to be x or y depending on which one is larger. And then we’ll process the rest of the list.

3:51
So let’s just try this code to see if it works. Sorry. I’m going to start an interactive prompt inside my emacs session and I”m going to run ghci. And I’m going to load max.hs which is where that code is stored. So I can say type maxhelper. There we go there. And I can run it– maxhelper. So if I give it a value to an empty list, it should return that value as the maximum.

4:29
If I give it a value that’s small, then I give it the list. Right. I’ve said minus 1. So I need to put that inside brackets. That’s interpreted correctly as an integer value. So maxhelper with an interim maximum of minus 1 and a list 1, 2, 3 returns 3, and so on. Let’s go back to the source code now and define a second function, which is called maxfromlist.

4:58
This is only going to take an integer list and it’s going to return the maximum value in that list. It’s going to use maxhelper to find the maximum value. So maxfromlist. If it’s supplied with an empty list what’s it going to return? I’ve got a problem here. There is no maximum to return. So let’s return nothing. And let’s say our return type has now become Maybe int. Because we can only return an int if there is a maximum. And if there isn’t, we’ll have to return Nothing. Maxfromlist. If there are elements in list then it’s going to return maxhelper. I need an equal sign there. Sorry. Equals maxhelper x. The interim maximum xs– the rest of the list.

5:56
And I need to wrap this up inside the Maybe. So I need to say it’s going to return Just this integer value. Good. So let’s go back to our ghci session. And let’s say, reload. Which will reload the edited max.hs source code. And now I should be able to say maxfromlist 1 2, 3, And that returns Just 3. Good. If I say maxfromlist empty list it returns Nothing. Great. This works fine. The Maybe type means that the type checker guarantees client code will check the result of the computation. Probably with a case expression. This is much more robust than C error codes.

6:48
Right. Once we’ve generated Maybe values, how do we propagate them in programs? Perhaps by using lots of case statements. But surely there must be a better way. And there is. At this point I will mumble the word monads. But we’ll cover these in more detail in week six. For now let me show you how you can map functions over Maybe values. So let’s say let function inc equals one plus.

7:25
So the type of inc in the context of the type variable a that is an instance of the number type class. Then inc takes an a and returns an a. So if I say inc 1 it returns the value 2. How about if I say inc Just 1? Look. It doesn’t work. So inc can’t be applied to Maybe values. However, if I use the fmap function– this is a higher order function here– inc Just 1. I get back Just 2. So fmap allows inc to be applied to the value inside the Maybe. The 1 inside the Just 1. Fmap inc Just 2 returns Just 3. And now you’re saying, what happens if I apply inc 2 Nothing? Guess.

8:40
I get back Nothing. Great. Let’s leave Maybes there for now. We’ll come back to them and monads in general later in the course. Thanks. Bye.
```

**What do we do when computations fail to generate results? Examples include taking the head of an empty list, or finding the minimum value of a tree which turns out to be a `Leaf`.**

Haskell provides `Maybe` values, which allow us to denote missing results with `Nothing`. This is type-safe, so much better than `null` in C-like languages. Maybe is like `Option` in Scala.

### Idiomatic Haskell

#### Question 1

```hs
let x = y + 2
    y = x/3
in x+y
```

This code defines a pair of _simultaneous_ equations. Will this work in Haskell?

- [ ] yes —- it will compute the two values that have mutually recursive definitions
- [x] no — it will either fail with an error or loop forever.

#### Question 2

What is the missing case clause in the following definition of a function to calculate the length of a Haskell list?

```hs
mylength l =
case l of
  -- MISSING CLAUSE --
  x:xs -> 1+mylength xs
```

- [x] `[] -> 0`
- [ ] `Null -> 0`
- [ ] `[] <- 0`

#### Question 3

In a Haskell guard expression, each of the guards evaluates to a `Bool` value, either `True` or `False`. What is the `Bool` value for the `otherwise` case?

- [ ] False
- [x] True
- [ ] Maybe True

#### Question 4

Select which one of the following two `let` expressions will evaluate to the `String`

```hs
"prime minister"
```

- [x] `let x = numeral ++ " minister"  where numeral = "prime" in x`
- [ ] `let x = numeral ++ " minister"  in x where numeral = "prime"`

#### Question 5

Study the Haskell function `f` below. What does `f()` evaluate to?

```hs
f :: () -> String
f () = let x = (Just Nothing)
       in
        case x of
         (Just _) -> "something"
         Nothing -> "nothing"
```

- [x] `"something"`
- [ ] `"nothing"`
- [ ] Nothing

## Parsing Text

In this activity you will learn how to use Haskell for parsing of text.

### Parsing Text Using Higher-Order Functions

#### The Parsing Problem

Parsing is the mechanism we use to make sense of structured information, e.g. written or spoken language. In the case of written language, it involves several steps:

- recognizing the characters of the writing system,
- identifying words,
- identifying sentences,
- identifying paragraphs etc.

To be able to do so we need to know the writing system, spelling and grammar of the language in which the document is written.

For parsing structured text such as program source code, HTML or JSON, the problem is similar.

#### Some handy functional machinery

##### Returning functions as values

- So far, we have seen functions that take functions as arguments
- Functions can also return functions as values
- For example, partial application of a function:

```hs
sum = foldl (+) 0
```

Here (sum) is the result returned by partial application of (foldl).

- More explicitly, we can write this as:

```hs
sum = \xs -> foldl (+) 0 xs
```

Here (sum) is a function resulting from partial application of (foldl).

- Both are of course the same thing, just different interpretations.

##### Function generators

- We can use this concept to generate parameterised functions

For example, the following function generates functions that add a constant number to their argument:

```hs
gen_add_n = \n ->
    \x -> x+n

add_3 = gen_add_n 3
add_7 = gen_add_n 7

add_3 5 --> 8
add_7 4 --> 11
```

- This is of course not limited to numeric constants

For example, the following function generates functions that perform a given arithmetic operation on a constant number and their argument:

```hs
gen_op_n = \op n ->
    \x -> x `op` n

add_3 = gen_op_n (+) 3
mult_7 = gen_op_n (*) 7

add_3 5 --> 8
mult_7 4 --> 28
```

#### Practical parsing

##### Cooking soba noodles

To make the parsing problem more concrete, suppose you have to parse the following recipe and identify the different steps required in the preparation.

> Bring a large pot of water up to a boil. Unlike Italian pasta, you do not need to salt the water. Once it’s boiling, hold the noodles over the water and sprinkle them in strand by strand. Once all the noodles are in, stir gently so that they are all immersed in the water. Bring the water back up to a gentle boil, then lower the heat so that the water is just simmering. (This differs from the ’rolling boil’ that’s recommended for pasta.) If the water threatens to boil over, add about 1/2 cup of cold water (but if you lower the heat to the gentle simmer, and have a big enough pot, this shouldn’t be necessary). Cook for about 7 to 8 minutes, or following the package directions (for thinner noodles 5 to 6 minutes may be enough. Test by eating a strand - it should be cooked through, not al dente, but not mushy either).

##### Parsing text

1. Typically, a functional program is organised around a tree-like data structure with an algebraic data type that represents the core data
2. A parser reads text input and generates the tree
3. Functions perform transformations or traversals on the tree
4. Pretty-printer functions output the tree (original or transformed)

##### Alternative approaches to parsing

- Don’t bother with parsing, just make the user provide input in an awkward form. _A common approach, but please don’t do this!_
- Write the parser by hand, with just ordinary list processing functions. Possible, but hard and not reusable. Don’t do it.
- Write the parser using _regular expressions_. Tempting but limiting and not reusable. Don’t do it, unless your input text format is very simple.
- Use _parser combinators_. For most purposes, this is the recommended approach, for everything from basic input formats to medium size programming languages (e.g. Pascal) or subsets of languages like C and Fortran.
- Use a parser generator, e.g. _yacc, bison, antlr, happy_. The best approach for heavy-weight parsers, for very large programming languages.

##### Parser combinators

- Parser combinators are functions that allow you to combine smaller parsers into bigger ones.
- They are higher-order functions that take functions as arguments and return functions
- A parser combinator library provides both basic parsers (for words, numbers etc.) and combinators.

##### Parsec: monadic parsing combinators

- There are many parsing libraries for Haskell.
- One of the most widely used is Parsec, which is robust, flexible, expressive, and efficient.
- Parsec operates in a monad.

##### A Quick Primer on Monads

You may have heard the term _monad_ before, and we will discuss the concept in detail in a later session. Haskell uses monads to structure computations. You have already encountered the IO monad, which you need to use to perform IO in a Haskell program. A typical example is

```hs
hello :: String -> IO String
hello x =
  do
     putStrLn ("Hello, " ++ x)
     putStrLn "What's your name?"
     name <- getLine
     return name
```

This illustrates the key syntactic features of a monad: the `do` keyword, the sequence of commands, the way to extract information from a monadic computation using the left arrow `<-` and the `return` keyword. In fact, using the do-notation is quite similar to imperative programming.

Also note the return value of our _hello_ function: not just _String_ but _IO String_. A computation done in a monad returns a “monadic” type, we say that the string is returned inside the monad.

##### Form of a parser

- For example, suppose we want to parse a string of the form (< tag>), where (tag) must be a word, and return the tag as a type (Tag).

To run the code below you need to import some libraries and add some additional code. All this is explained in detail in the [tutorial](https://www.futurelearn.com/courses/functional-programming-haskell/10/steps/1103600); the full source code is on [GitHub](https://github.com/wimvanderbauwhede/HaskellMOOC/tree/master/ParsecTutorial).

```hs
data Tag = MkTag String

parseTag :: Parser Tag
parseTag =
  do  char '<'
      x <- identifier
      char '>'
      return (MkTag x)
```

As you can see, the parser consists of a number of functions (e.g. _char_ and _identifier_) that are called sequentially. Also, the return value is of type _Parser Tag_, not simply _Tag_. This is because `parseTag` is not returning a _value_, instead it returns a _parser_. We can combine this parser with other parsers, and then we can execute the final parser on our data. We will cover this approach in more detail in the tutorial.

##### Testing a parser

To test your parser, start `ghci`:

```sh
[wim@fp4 ~]$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
```

Then, import Parsec:

```sh
Prelude> import Text.ParserCombinators.Parsec
```

Parsec provides the handy `parseTest` function, which takes a parser and a string and runs it. Let’s try and run the parser `char 'b'` on the string `"cons"`:

```sh
Prelude Text.ParserCombinators.Parsec> parseTest (char 'b') "cons"
Loading package bytestring-0.9.2.1 ... linking ... done.
Loading package transformers-0.2.2.0 ... linking ... done.
Loading package mtl-2.0.1.0 ... linking ... done.
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package text-0.11.2.0 ... linking ... done.
Loading package parsec-3.1.2 ... linking ... done.
```

Because the string “cons” does not contain the character ‘b’, we get a parse error:

```sh
parse error at (line 1, column 1):
unexpected 'c'
expecting 'b'
```

Let’s try with `char 'c'`:

```sh
Prelude Text.ParserCombinators.Parsec> parseTest (char 'c') "cons"
'c'
Prelude Text.ParserCombinators.Parsec>
```

This time the parse succeeded.

##### Running the parser

The actual code for the example `parseTag` requires some extra modules and definitions, this is covered in the [tutorial](https://www.futurelearn.com/courses/functional-programming-haskell/10/steps/1103600); the full source code is on [GitHub](https://github.com/wimvanderbauwhede/HaskellMOOC/tree/master/ParsecTutorial).

As a simple example, let’s define `parseDiv` as:

```hs
-- the "deriving Show" is needed to let `ghci` print the result
data Tag = MkTag String deriving Show

parseDiv = do
  string "<div>"
  return (MkTag "div")
```

To define this function in `ghci` you can write this on one line as follows:

```hs
let parseDiv  = do { string "<div>";return $ MkTag "div" }
```

Now we can run this parser using the `parseTest` function:

```sh
Prelude Text.ParserCombinators.Parsec> parseTest parseDiv "<div>"
Loading package parsec-2.1.0.1 ... linking ... done.
MkTag "div"

Prelude Text.ParserCombinators.Parsec> parseTest parseDiv "div"
parse error at (line 1, column 1):
unexpected "d"
expecting "< "
Prelude Text.ParserCombinators.Parsec>
```

##### Anatomy of a basic parser

- All parser combinators are functions that return functions.
- It is the returned function that operates on the string, not the parser combinator function.
- The basic parsers ((identifier),(natural),(char)) take either no arguments (e.g. (identifier)) or one or more strings for parametrisation (e.g. (char)).

```hs
char = \ch -> \str ->
      -- try to match the character ch
      -- return the result
```

If the match succeeds, the matching string is removed from the input string; otherwise, the original string is returned, e.g.

```hs
char "c" "cons" -->
"c"
char "b" "cons" -->
parse error at (line 1, column 1):
unexpected "c"
expecting "b"
```

##### Anatomy of a parser combinator

- Parser combinators such as `<|>` and `parens` take other parsers as arguments.

```hs
parens = \p ->
    \str ->
        -- first match "("
        -- perform the parse of p if "(" was found
        -- then match ")"
        -- return the result
```

##### Parsing alternatives

- Often we want to try one parser; if that fails, then try another one instead. The choice combinator `<|>` provides this functionality.
- Example: (letter\_digit) will match either a letter or a digit.

```hs
letter_digit :: Parser Char
letter_digit =
  do  x <- letter <|> digit
      return x
```

###### Running alternative parsers

```sh
Prelude Text.ParserCombinators.Parsec> parseTest letter_digit "b2"
"b"

Prelude Text.ParserCombinators.Parsec> parseTest letter_digit "2b"
"2"

Prelude Text.ParserCombinators.Parsec> parseTest letter_digit "*2"
parse error at (line 1, column 1):
unexpected "*"
expecting letter or digit
```

##### Parsing alternative strings

Suppose we want to match either _bag_ or _bog_, but nothing else.

```hs
bag_bog :: Parser String
bag_bog =
  do  xs <- string "bag" <|> string "bog"
      return xs
```

###### Failed alternative consumes input

So far so good:

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog "bag"
"bag"
```

And a non-matching string fails, as expected.

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog "bug"
parse error at (line 1, column 1):
unexpected "u"
expecting "bag"
```

But there’s a problem!

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog "bog"
parse error at (line 1, column 1):
unexpected "o"
expecting "bag"
```

The first parser _string “bag”_ matched the `b` but then failed on the `a`. _It has now consumed the `b`._ The second parser _string “bog”_ now tries to match `b` against `o`, which of course fails.

##### _try_ — don’t consume input on failed parse

To allow you to parse tentatively without consuming any input, Parsec provide the `try` function:

```hs
bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs
```

###### Trying a parse without consuming input

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog_try "bag"
"bag"
```

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog_try "bug"
parse error at (line 1, column 1):
unexpected "u"
expecting "bog"
```

```sh
Prelude Text.ParserCombinators.Parsec> parseTest bag_bog_try "bog"
"bog"
```

##### Some parsers from the library

The Parsec library provides some small parsers that are useful for defining bigger ones:

- (char\\; “?”) — (char) is applied to a character, and it gives a parser that matches that character
- (letter) — matches any letter
- (digit) — matches any digit
- (string) — matches a string of characters
- (stringLiteral\\; “xyz\*”) — matches the string argument
- (many\\; p) — matches 0 or more occurrences of parser (p)
- (many1\\; p) — matches 1 or more occurrences of parser (p)

##### Variable names

```hs
varname :: Parser String
varname =
  do  x <- letter
      xs <- many (letter <|> digit)
      return (x:xs)
```

```sh
Prelude Text.ParserCombinators.Parsec> parseTest varname "a4cc7*5"
"a4cc7"
Prelude Text.ParserCombinators.Parsec> parseTest varname "34a"
parse error at (line 1, column 1):
unexpected "3"
expecting letter
```

##### Expression parsers

- Arithmetic expressions are complex to parse because of the rules of precedence and the arity of the operators.
- Parsec provides support for expression parsing, so you don’t have to write your own expression parser.

```hs
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

optable =
  let
    op name assoc   =
      Infix ( do {  reservedOp name;
          return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
    prefix name =
      Prefix  (
        reservedOp name >>
            return (\x->(Pref (MkPrefixOpExpr name x))) )
  in
    [ [ op "*"  AssocLeft, op "/"  AssocLeft, op "%" AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ], [ prefix "-" ] ]
```

This example uses some additional monad syntax: you can use braces and semicolons instead of indentation; and the `>>` operator is also a shorter way of writing the do notation:

```hs
do
  expr1
  expr2
```

can be written as

```hs
expr1 >> expr2
```

Also note the use of the `<?>` operator, this is used to define a custom error message in case a parse fails without consuming any input. This is a very useful debugging feature.

- Parsec also has support for programming languages with a mechanism to define the syntax and keywords through _makeTokenParser_.
- For simple cases, you can use _emptyDef_.

```hs
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
commaSep        = P.commaSep lexer
-- and many more
```

### Parsing using Parsec: a practical example

```
0:05
WIM: In this tutorial, I want to explain how you can use the Parsec parser combinator library for practical parsing tasks.

0:16
You have learned before how to use the Show typeclass and the show function to print out Haskell data structures. So in this tutorial, I want to show you how you can create a parser for the output from the Show function and then turn it into XML, for example.

0:36
So let’s start by creating a few data structures.

0:43
So we create a data structure PersonRecord just as an example, where we have the name of the person, the address, some integer identifier, and a few labels. So the Address is a data structure in its own right and so are the labels. They are an algebraic data structure like this.

1:08
Note that we have used deriving Show so that you don’t have to write your own Show function. By using this deriving clause, GHC will automatically create the show function for these data structures. And it is the output of this automatically created show function that we are going to parse. So let’s create a few instances of this data type that we have here. So let’s say record 1 is, and record 2. And now we can use show on these two records, and this is the output that show gives us. So it’s this output that we now want to transform into XML using the Parsec library. So let’s go and build a parser using the Parsec library.

2:17
I’m creating a module showParser, which exports a single function ParseShow, which is going to parse the output from Show and turn it into XML. To use Parsec in practice, there are a number of modules that you usually need. First, we have the actual parser combinators, then we have a library of token parsers, and then we have a handy support library called Parsec.Language, which defines the basics of the programming language by defining what is an identifier, what is a comment, and things like that. But the Parsec.Token library exports a function makeTokenParser and this function takes a language definition.

3:01
Now, because we’re not really having a programming language where we use emptyDef, if you want to know more about the language definitions, I suggest you read the Parsec manual. So then, given this lexer, so basically, the definition of what are identifiers and so on in our system, we can now build a number of parsers very conveniently like this. So the library finds a number of parsers for a given lexer, and now you can actually parse parentheses, brackets, braces, comma-separated lists, white spaces, symbols being predefined strings, identifiers being whatever is an identifier in your language, integers, and so on. There are a lot more of them, and again, you can look at the Parsec library.

3:48
So with this boilerplate, we can start making the actual parser, and this will be a parser that takes a string, being the output from the Show function, and it produces a new string, which is the XML representation of this input string. So we have a function with signature, ParseShow, string to string. And the way this works is that we have a function that creates– that is our actual parser, and this parser is a monadic parser, which means, basically, that it consists of functions that you put together. So you actually produce one big combined function, and then you apply that function to the string. And to apply this function to the string, we use another function, run_parser, that looks as follows.

4:43
So run_parser is independent of the actual input of the parser. So you have a parser of a certain data type and a string, and it produces this output data type that you desire. So in our case, the data type that we desire is a string, so see here is a string. And what it does is it applies our parser to an input string, and then if there’s an error, it reports an error. Or if there’s no error, it returns the result. So this function, run_parser, is kind of generic. So you can always reuse it. In our case, the parser that we want to build, we will call showParser, and that’s a parser of string. So it returns a string.

5:28
So with this showParser and run_parser, we can now build our actual ParseShow function. So we say, ParseShow equals simply like this. So maybe we want to make this a little bit more complete. To emit valid XML, we can tag this with an actual XML header string. So let’s say, we define the XML header string as, and then we can actually change this function a little bit.

6:07
In the opening tag, we must actually create, not just the name of the tag, but also the list of attributes. So the list of attributes is a list of tuples for the name of the attribute and the value of the attribute. And so what we do is we apply a map of a function that turns this tuple into a string, where we have the key, and then equals quote, , value, quote. Now, so this approach here for string concatenation is very convenient, rather than using the ++ in terms of readability. So concat will simply concatenate all of the elements of a list. With these tag manipulation functions, we are almost ready to create the actual parses.

6:52
There’s another function that is very handy to use. It’s a function that will join elements of a list using newline. So let’s build that function. We have join. And I use the function intercalate. So intercalate is a function that takes all the elements of a list and joins them together, interspersed with the particular character that is the first argument of this function. But this function intercalate is not part of the standard prelude, so we have to add it to our import list here. With all this machinery ready, we can now create the rest of our parsers.

7:32
For instance, if we want to parse a list, so we parse brackets, and then a comma-separated value of whatever can be inside the list till we call showParser again. And the result is the list of the elements that we parsed. What we do then is we tag this list with the type list, and then we join them with newlines, and each element is surrounded by a tag list element. The other parsers are quite similar. So I’ll just put them all here. We have a tuple_parser, which parses parentheses and then comma-separated. So that’s a tuple, and then drops it into tuple tags.

8:15
We have the record_parser, which first parses the name of the record, and then it has braces, and then it has a comma-separated list of key values. So for key values, I have an additional parser, kvparser, here, which parses an identifier, equal sign, and then whatever. And then it returns this wrapped in a tag element, and then it has keys and values. And finally, we parse the type_identifier. So type_identifiers are defined as strings that start with an uppercase letter and then one or more alphanumeric letters. This is our Parser that we use to parse the labels. So with all these things together, we can actually parse a show output and turn it into XML.

9:00
So of course, we need to change our main program a little bit to make that happen. First of all, we must import the parser that we just wrote. And then, obviously, we must use it, but that’s very simple. So we change the main program just a little bit.

9:22
So we call show on the records that we created and put the result in the record string. We call ParseShow on the record string and that’s our main program.

9:35
So now we can try this out. And, indeed, we have created an XML document that represents the output of show called on our custom data structure. So this is an example of how you can use Parsec to easily transform strings into different strings, for instance, XML documents.
```

The aim of this tutorial is to explain step by step how to build a simple parser using the Parsec library.

The source code can be found on [GitHub](https://github.com/wimvanderbauwhede/HaskellMOOC/tree/master/ParsecTutorial)

#### Parsing the output of derived `Show`

The polymorphic function `show` returns a string representation of any data type that is an instance of the type class `Show`. The easiest way to make a data type an instance of type class is using the `deriving` clause.

```hs
    show :: Show a => a -> String
    data D = D ... deriving (Show)
    d :: D
    d = D ...
    str :: String
    str = show d -- string representation of the instance of the data type
```

In this tutorial we will show how to create a parser that will parse the output of a derived `show` and return it in XML format.

```hs
    parseShow :: String -> String
    xml = parseShow $ show res
```

#### Example data type

First we create a data type `PersonRecord`

```hs
data PersonRecord  = MkPersonRecord {
    name :: String,
    address :: Address,
    id :: Integer,
    labels :: [Label]
} deriving (Show)
```

The types `Address` and `Label` are defined as follows:

```hs
data Address = MkAddress {
    line1 :: String,
    number :: Integer,
    street :: String,
    town :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)
```

We derive `Show` using the `deriving` clause. The compiler will automatically create the show function for this data type. Our parser will parse the output of this automatically derived show function.

Then we create some instances of `PersonRecord`:

```hs
rec1 = MkPersonRecord
    "Wim Vanderbauwhede"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    557188
    [Green, Red]

rec2 = MkPersonRecord
    "Jeremy Singer"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    42
    [Blue, Yellow]
```

We can test this very easily:

```hs
main = putStrLn $ show [rec1,rec2]
```

This program produces the following output:

```sh
    [wim@workai HaskellParsecTutorial]$ runhaskell test_ShowParser_1.hs
    [MkPersonRecord {name = "Wim Vanderbauwhede", address = MkAddress {line1 = "School of Computing Science", number = 17, street = "Lilybank Gdns", town = "Glasgow", postcode = "G12 8QQ"}, id = 557188, labels = [Green,Red]},MkPersonRecord {name = "Jeremy Singer", address = MkAddress {line1 = "School of Computing Science", number = 17, street = "Lilybank Gdns", town = "Glasgow", postcode = "G12 8QQ"}, id = 42, labels = [Blue,Yellow]}]
```

The derived Show format can be summarized as follows:

- Lists: \[… comma-separated items …\]
- Records: { ... comma-separated key-value pairs ...}
- Strings: “…”
- Algebraic data types: variant type name

#### Building the parser

We create a module `ShowParser` which exports a single function `parseShow`:

```hs
module ShowParser ( parseShow ) where
```

##### Some boilerplate

```hs
    import Text.ParserCombinators.Parsec
    import qualified Text.ParserCombinators.Parsec.Token as P
    import Text.ParserCombinators.Parsec.Language
```

The `Parsec.Token` module provides a number of basic parsers. Each of these takes as argument a _lexer_, generated by `makeTokenParser` using a language definition. Here we use `emptyDef` from the `Language` module.

It is convenient to create a shorter name for the predefined parsers you want to use, e.g.

```hs
    parens = P.parens lexer
    -- and similar
```

##### The parser

The function `parseShow` takes the output from `show` (a `String`) and produces the corresponding XML (also a `String`). It is composed of the actual parser `showParser` and the function `run_parser` which applies the parser to a string.

```hs
    parseShow :: String -> String
    parseShow = run_parser showParser

    showParser :: Parser String

    run_parser :: Parser a -> String -> a
    run_parser p str =  case parse p "" str of
        Left err -> error $ "parse error at " ++ (show err)
        Right val  -> val
```

##### The XML format

We define an XML format for a generic Haskell data structure. We use some helper functions to create XML tags with and without attributes.

```htm
<?xml version="1.0" encoding="utf-8"?>
```

```hs
    xml_header =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
```

###### Tags

```htm
<tag> ... </tag>
```

```hs
    otag t = "<"++t++">"
    ctag t = "</"++t++">"
    tag t v = concat [otag t,v,ctag t]
```

###### Attributes

```htm
<tag attr1="..." attr2="...">
```

```hs
    tagAttrs :: String -> [(String,String)] -> String -> String
    tagAttrs t attrs v =
        concat [
            otag (unwords $ [t]++(map (\(k,v) -> concat [k,"=\"",v,"\""]) attrs))
            ,v
            ,ctag t
            ]
```

We also use some functions to join strings together. From the Prelude we take:

```hs
    concat :: [[a]] -> [a] -- join lists
    unwords :: [String] -> String -- join words using spaces
```

We also define a function to join strings with newline characters:

```hs
    joinNL :: [String] -> String -- join lines using "\n"
```

This is identical to `unlines` from the Prelude, just to illustrate the use of `intercalate` and the `Data.List` module.

##### Parsers for the derived `Show` format

###### Lists

```hs
[ ..., ..., ... ]
```

XML :

```xml
<list>
<list-elt>...</list-elt>
...
</list>
```

```hs
    list_parser = do
        ls <- brackets $ commaSep showParser
        return $ tag "list" $ joinNL $ map (tag "list-elt") ls
```

###### Tuples

```hs
: ( ..., ..., ... )
```

XML:

```xml
<tuple>
<tuple-elt>...</tuple-elt>
...
</tuple>
```

```hs
tuple_parser = do
    ls <- parens $ commaSep showParser
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls
```

###### Record types

```hs
Rec { k=v, ... }
```

XML:

```xml
<record>
<elt key="k">v</elt>
...
</record>

key-value pairs: k = v -- v can be anything
```

```hs
record_parser = do
    ti <- type_identifier
    ls <- braces $ commaSep kvparser
    return $ tagAttrs "record" [("name",ti)] (joinNL ls)

kvparser = do
    k <- identifier
    symbol "="
    t <- showParser
    return $ tagAttrs "elt" [("key",k)] t

type_identifier = do
    fst <- oneOf ['A' .. 'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest
```

###### Algebraic data types

```hs
e.g. Label
```

XML:

```xml
<adt>Label</adt>
```

```hs
adt_parser = do
    ti <- type_identifier
    return $ tag "adt" ti
```

###### Quoted strings and numbers

```hs
quoted_string = do
    s <- stringLiteral
    return $ "\""++s++"\""

number = do
    n <- integer
    return $ show n
```

##### Complete parser

Combine all parsers using the choice combinator `<|>`.

```hs
    showParser :: Parser String
    showParser =
        list_parser <|> -- [ ... ]
        tuple_parser <|> -- ( ... )
        try record_parser <|> -- MkRec { ... }
        adt_parser <|> -- MkADT ...
        number <|>    -- signed integer
        quoted_string <?> "Parse error"
```

Parsec will try all choices in order of occurrence. Remember that `try` is used to avoid consuming the input.

#### Main program

Import the parser module

```hs
    import ShowParser (parseShow)
```

Use the parser

```hs
    rec_str =  show [rec1,rec2]
    main = putStrLn $ parseShow rec_str
```

Test it:

```sh
[wim@workai HaskellParsecTutorial]$ runhaskell test_ShowParser.hs
<?xml version="1.0" encoding="UTF-8"?>
<list><list-elt><record name="MkPersonRecord"><elt key="name">"Wim Vanderbauwhede"</elt>
<elt key="address"><record name="MkAddress"><elt key="line1">"School of Computing Science"</elt>
<elt key="number">17</elt>
<elt key="street">"Lilybank Gdns"</elt>
<elt key="town">"Glasgow"</elt>
<elt key="postcode">"G12 8QQ"</elt></record></elt>
<elt key="id">557188</elt>
<elt key="labels"><list><list-elt><adt>Green</adt></list-elt>
<list-elt><adt>Red</adt></list-elt></list></elt></record></list-elt>
<list-elt><record name="MkPersonRecord"><elt key="name">"Jeremy Singer"</elt>
<elt key="address"><record name="MkAddress"><elt key="line1">"School of Computing Science"</elt>
<elt key="number">17</elt>
<elt key="street">"Lilybank Gdns"</elt>
<elt key="town">"Glasgow"</elt>
<elt key="postcode">"G12 8QQ"</elt></record></elt>
<elt key="id">42</elt>
<elt key="labels"><list><list-elt><adt>Blue</adt></list-elt>
<list-elt><adt>Yellow</adt></list-elt></list></elt></record></list-elt></list>
```

#### Summary

- Parsec makes it easy to build powerful text parsers from building blocks using predefined parsers and parser combinators.
- The basic structure of a Parsec parser is quite generic and reusable
- The example shows how to parse structured text (output from Show) and generate an XML document containing the same information.

### Parser Puzzles

#### Question 1

What is the type of the function `json_parser`?

```hs
json_parser :: Parser __
json_parser = do
        whiteSpace
        j_top <- ( json_array_parser <|> json_obj_parser)
        return j_top
```

json_parser :: Parser `JValue`

#### Question 2

A JSON array consists of a comma-separated list of JSON values enclosed by braces, e.g.

```hs
[ 1, 'two',  [ 3, true] ]
```

To parse this format we use the function `json_array_parser` below. What is the correct data constructor for the return value?

```hs
json_array_parser :: Parser JValue
json_array_parser = do
    j_vals <- brackets $ commaSep json_value_parser
    return $ __ j_vals
```

return $ `JArray` j_vals

#### Question 3

The JSON format supports boolean values, named `true` and `false`.

In the boolean JSON value parser below, what is the missing combinator?

```hs
json_bool_parser = do
    bstr <- ( symbol "true" __ symbol "false" )
    let
        bval = if bstr == "true" then True else False
    return $ JBool bval
```

( symbol "true" `<|>` symbol "false" )

#### Question 4
A JSON object is a list of key-value pairs, where the key is a string and the value a JSON value, enclosed in braces, e.g.

```json
{ "Street" : "Lilybank Gardens",
  "Nr" : 18,
  "Org" : [
        "University of Glasgow",
        { "School" : "Computing Science"}
  ]
}
```

The most general `JValue` parser is `json_value_parser`, which is built of parsers for specific JSON values:

```hs
json_value_parser =
    json_array_parser <|>
    json_obj_parser <|>
    json_string_parser <|>
    json_number_parser <|>
    json_bool_parser <|>
    json_null_parser
```

In the JSON pair parser below, provide the name of the parser for the ‘value’ part of the pair

```hs
json_pair_parser = do
    k <- stringLiteral
    colon
    v <- __
    return $ mkJPair k v
```

v &lt;- `json_value_parser`

#### Question 5

In the JSON object parser below, complete the return expression.

```hs
json_obj_parser :: Parser JValue
json_obj_parser = do
    j_vals <- braces $ commaSep json_pair_parser -- a list of pairs
    return $ __ j_vals
```

return $ `mkJObj` j_vals

#### Question 6

Given the following parser:

```hs
yin_yang :: Parser String
yin_yang =
  do  xs <- string "yin" <|> string "yang"
      return xs
```

With the definition as above this parser will fail when trying to parse “yang”:

```sh
*Main> run yin_yang "yang"
parse error at (line 1, column 1):
unexpected "a"
expecting "yin"
```

How should you modify the parser so that it will work correctly?

- [ ]
```hs
    yin_yang :: Parser String
    yin_yang =
    do  xs <- string "yin" <|> try (string "yang")
        return xs
```

- [ ]
```hs
    yin_yang :: Parser String
    yin_yang =
    do  xs <- try (string "yin" <|> string "yang")
        return xs
```

- [x]
```hs
    yin_yang :: Parser String
    yin_yang =
    do  xs <- try (string "yin") <|> string "yang"
        return xs
```

### Summary

Parsing text is a very common and important operation. _Monadic parser combinators_ are a convenient mechanism to build parsers by combining building blocks, and a good illustration of the practical use of a monad. (We will cover monads in more detail in week 6.)

Each parser is a higher-order function that returns a function. The parser combinators combine these functions into the final parser.

## Am I Right?

The Quickcheck tool allows us to test the correctness of our Haskell programs.

### Check my Program is Correct

```
0:05
JEREMY: In today’s video, we’re going to find out about a powerful, automated testing tool called QuickCheck. This allows us to run randomly generated tests on our code to ensure the code meets certain correctness properties that we specify. QuickCheck comes as part of the Haskell platform, so you should already have access to it on your own machine. It’s widely used in the software industry. For instance, many modern telecom software products are tested using QuickCheck. OK, suppose we want to implement a substitution cipher. Let’s do Caesar’s cipher, where we encode letters by shifting them a certain number of places along in the alphabet. In this example here, we shift letters one place.

1:04
So add– A-D-D in the plaintext– will become bee– B-E-E– in the ciphertext. We might ask what plaintext Z, the final letter of the alphabet, should be in code. The answer is a. The alphabet wraps round. Properly, this is a rotation rather than a shift.

1:27
First, we import the Data.Char module, which allows us to use utility functions for manipulating characters.

1:40
We select candidate characters to cipher or encode using the shouldcipher function here. Look at the type signature. It’s going to take a character, a single character, and return a Boolean that tells us whether or not this particular character is going to be enciphered. And we’re only going to want to cipher characters c that are letters and are ASCII character codes. Haskell actually uses the Unicode character set. So this means we’re only going to encipher letters, alphabetic letters, and not numbers or punctuation characters. To encipher letters, then come down to the next function here, cipherchar. We want to move letters along in places in the alphabet, where the number of places we move them is the first argument shift.

2:40
That’s an integer. And the second argument is the original character, the character in the plaintext. And we’re going to return the character that’s the ciphered version of our plaintext character. So let’s think about how we’re going to do this. What we really want to do is to add a certain number of character positions, the shift, to the original character. But c is a character, and shift is an integer. So actually, we have to use the ord function to turn the character into an integer code. And then add in the shift and then use the chr function to turn the resulting integer back into a character. Type safety is important here. We can’t add integers and characters directly.

3:37
Ord and chr might be familiar to Python programmers, for instance. Now we only want to do this in the situation where the character– that is, the input c– is going to be a character that’s a letter that we ought to cipher. So let’s say shouldcipher c means that we do this calculation. And otherwise, we’re going to say, just return the value c itself. It might be a punctuation character, or a space, or another non-letter character.

4:20
Great, now what we want to do is to– let’s get rid of this comment– map this cipherchar function over the whole of the string. And that’s what the next function does. Cipher, do you see here? It takes an integer shift amount, a character list, i.e. a string. That’s the plaintext. And returns another character list, which is the ciphertext. So here’s the definition here. We’re going to use the map function. And what we’re mapping is the cipherchar function for a certain shift amount mapped onto every character in the plaintext. Let’s try out this code. I’m going to save it.

5:07
So let’s try this inside GHCi. First, we’ll load cipher.hs. That’s in the code directory. And then we’re going to say cipher hello. That’s my string. And we want to move things along one space. Sorry, one space. There we are. And we get the enciphered message. And then we’ll do an original Caesar message. He’d normally move things three places long. Veni, vidi, vici. And we get back a rather unpronounceable string there, which is its encoding. Now for the decipher, we want to shift letters back in the opposite direction to the encipher. Look– so decipher is going to have the same type as decipher. It’s going to take an integer shift amount.

6:03
The second parameter is going to be the character amount shift. And then it’s going to return to the decoded, deciphered message.

6:15
So decipher shift ciphertext is going to be– well, it’s really just the ciphering function, but shifting in the opposite direction. But hopefully, this ought to give us the deciphered or decrypted message. So let’s say reload. And now we should be able to use the decipher function. If I say, decipher 3, and then I’m going to copy and paste this encoded string here. I should get back the original message. And I do. Now we want to check that our code works properly. Of course we know it shouldn’t because we haven’t done the wrapping around the alphabet for Z.

7:15
Rather than devise a number of test cases, we will use the QuickCheck tool to generate random input data and see whether a correctness property holds for our code. The property we want to check is, if we cipher a message and then decipher it, we should get back the original message. First, let’s import the QuickCheck module. Test.QuickCheck.

7:40
Make sure you get the capital letters right. Notice how the interactive prompt now has been modified to include Test.QuickCheck to show that it’s loaded into our environment. Now let’s ask QuickCheck to check our property. We need to encode the property in Haskell. So suppose we cipher a string by shifting it n places. The string is called s. And then we decipher this string using the same shift amount. Then the answer we ought to get back should be equal to the original string itself.

8:20
This is the property we want to check. We need to wrap this up as a lambda abstraction. So we’re going to take n, which is an integer, and then s, which is a string.

8:37
And given these two parameters, we can then check that when we decipher the enciphered message, we get back the original message. Let’s put some types here. So we take an int, n, the string, s. And we’re going to return a Boolean, which is the result of the equality check. OK.

9:03
And we’re going to provide this lambda abstraction as an argument to the QuickCheck function. And look, it runs, and then it says it failed. And it’s failed after six tests. And here’s where it’s failed. Look. The shift amount is one, and the string we’re trying to cipher is z. If we shift z along one place– well, let’s do that and see what happens. cipher 1 z. And then we end up with the next character in the character encoding, which is open curly brace. And now, if we try and decipher this– decipher 1 open curly brace, it doesn’t go back to z, because look– this is not a letter. And we’re only ciphering letters.

9:59
And the definition of decipher 1 curly brace is of course cipher minus 1 curly brace. And ciphering doesn’t work for non-letter characters. So this is the problem. Our code fails because it doesn’t wrap around the alphabet. Our cipherchar doesn’t do the adjustment for wrapping. So QuickCheck showed us that our program is wrong. Let’s go back and write a correct Caesar cipher code. And in fact, I should have one a little bit further down here. So we need to check to see whether our character, when it’s shifted, is going to wrap around the alphabet. So it’s a lower character. And it’s character code is greater than z when we’ve done the shift. Then yes, we need to wrap around.

10:50
If it’s an upper case character, and its character code, when we add the shift in, is greater than an upper case Z, Then yes, we need to wrap. Otherwise, we don’t. So that’s the wrap around. And then this other function here I’ve called bettercipherchar takes an integer shift amount, a character to encode, and then the encoded character. And this does some adjustments to make sure that we wrap around the alphabet if we need to. You can download this code and check it for yourself later on. It’s linked at the bottom of this page in the course. But what I’m going to do is change the definition of cipher to use bettercipherchar, rather than the original cipherchar.

11:41
And now, hopefully when I go back to shell and say reload, and then try and test my property again with this bettercipherchar, it ought to work now. So I want to QuickCheck this expression. Lambda n, that’s my shift amount. Lambda s, that’s my string. And I want to check that when I decipher the ciphered message, then the thing I get back is the original message.

12:26
I’m going to put in the types again. The types are helpful for QuickCheck to generate the right kind of random input data. Let’s run this now, and hopefully, it ought to pass. Look, it says it passed 100 tests, which gives us some confidence that it might be correct. We can actually trace the QuickCheck tests to see what the randomly generated input data looks like. Instead of using the QuickCheck function– I’m just going to borrow that code there. Instead, we use the verboseCheck function instead. VerboseCheck. Let’s run that, and we’ll see it generates the input data. There it is scrolling up there. It says for each test whether it passes.

13:14
So in this last example here, the shift amount is minus 42. And here’s the string, very random string here that was generated. Some of the characters aren’t even printable. But it passes all the tests. Great, we’ve passed the tests. A word of warning– even though the property is correct for all tests, this doesn’t guarantee our code is correct. Edsger Dijkstra, a famous computer scientist once said, testing can only show the presence of bugs, not their absence. However, QuickCheck provides a nice way to generate automated random tests to help us have more confidence that our code is doing what we want it to do.
```

**QuickCheck is a useful tool for automatically generating test cases for your Haskell programs. It’s widely used in industrial settings.**

In this video, Jeremy is checking his Caesar’s Cipher implementation. You can download this code (see related links) and run QuickCheck for yourself.

### Using QuickCheck

**Now let’s have a go at using QuickCheck for ourselves. We presume you have already watched the video about QuickCheck in the previous step.**

We’ll run this session in GHCi — so we can type things interactively and see how we get on.

Let’s start by checking some properties of the `abs` function, that returns the absolute value of a number.

For positive numbers, the absolute value of a number is equal to itself, we could try to check this property.

```hs
import Test.QuickCheck
quickCheck ((\n->(abs(n) == n))::Int->Bool)
```

It’s helpful to put the types in here. We have created a lambda function that takes a single integer parameter then checks to see whether it is equal to its absolute value. When we run QuickCheck, we see this fails quickly — for negative numbers! This is a problem with our property specification, not with the `abs` function itself.

OK — so let’s write another check. We want to say that a value is always equal to its absolute value, or (0 - its absolute value)

```hs
quickCheck ((\n->(abs(n) == n) || (0-abs(n) ==n))::Int->Bool)
```

Great, it works! Now what else could we check? Maybe we could that the first element of a sorted list is its minimum, or that the last element is its maximum?

```hs
import Data.List
quickCheck ((\l->((minimum l) == (sort l)!!0))::[Int]->Bool)
```

Unfortunately this property fails for empty lists. So let’s refine our definition so it’s always true for empty lists.

```hs
quickCheck ((\l->(if l==[] then True else (minimum l) == (sort l)!!0))::[Int]->Bool)
```

Now it works — at least, it passes 100 tests. We can actually discover what these tests are — use `verboseCheck` instead of `quickCheck`.

Now we see why we had to specify the type for the list. This means QuickCheck generates random lists of the right type. Let’s try `Char` lists instead:

```hs
verboseCheck ((\l->(if l==[] then True else (minimum l) == (sort l)!!0))::[Char]->Bool)
```

This works fine, although most of the characters are unprintable.

I guess you might want to use QuickCheck for some of your own code — it’s very helpful for prototyping!

The QuViC company makes money using QuickCheck for various telecoms projects, see [their website](http://www.quviq.com/successes/) for details.

### Talk with a Haskell Teacher

```
0:07
JEREMY: Hi, Graham. Welcome to Glasgow. Good to see you. We should give your official title, Professor Hutton. Now I think you teach first years to program in Haskell in Nottingham University, don’t you?

0:22
GRAHAM HUTTON: Yes.

0:23
JEREMY: Do you think Haskell is a good first programming language to learn?

0:27
GRAHAM HUTTON: I think I think it depends on the individual. What we find in Nottingham is some people who’ve done lots of programming before, it’s kind of hard to unwire their brain and get them to forget the imperative, loop-based, variable-based way of thinking.

0:43
JEREMY: Which is totally different to our functional style.

0:46
GRAHAM HUTTON: Which is totally different. And it takes a while for them to undo their brain. Whereas some people who have never seen any programming language before, they can get into the Haskell way of thinking straight away. And what we tend to find is people who can think abstractly, they’re the ones who can pick it up very, very easily.

1:03
JEREMY: Yeah. OK. So there are certain notions that people really struggle with like recursion, for instance or other kinds of functional concepts that you wouldn’t see in an imperative language so readily. What do you do when people say, I don’t get this? How do you kind of coax them into a functional mind set?

1:23
GRAHAM HUTTON: So I’ve thought about this for quite a long time because I’ve been teaching Haskell to the first years and second years for about 20 years. What I do with the students is three things. I say you should be very systematic about your learning. So when I teach Haskell, I teach it in a very kind of language-focused way at the start. So I teach the very basic concepts, the functions, list comprehensions, recursion, and so on. And I very much see this as a tower. You learn the simple things, then a little bit more, and then a little bit more. And the second thing I do to the students is say you must actually look at larger programming examples.

1:55
So in the new book, I’ve got about 20 larger programming examples. And I go through some of those with the students. And they see kind of code written by somebody who’s been doing this a long time. And I hope it gets them up to speed. And last thing, of course, I say to the students is you need to do a lot of programming yourself. So we sent them lots and lots of exercises in the labs and with course-works and so on. And they build up to writing larger programs themselves.

2:20
JEREMY: I think that’s good. There’s got to be a balance between reading code and writing code yourself.

2:24
GRAHAM HUTTON: Yes.

2:24
JEREMY: Both things are important–

2:25
GRAHAM HUTTON: Absolutely.

2:25
JEREMY: –to build an understanding of language. That’s good. That’s what we’re trying to do in our course. Now you mentioned your new book there. This is the Glasgow leg of your book tour.

2:33
GRAHAM HUTTON: Yes.

2:34
JEREMY: Can you tell us something about your Haskell textbook?

2:37
GRAHAM HUTTON: Yes. So this is the new book, which is just out. So I’ve been teaching Haskell for 20 years. So I’ve got lots of experience with this. And I love Haskell. And I love teaching it to people. And I want everyone to learn Haskell. So that’s why I’ve produced a second edition of the book. It’s divided into two parts. It’s got some introductory material in the first half of the book.

2:59
JEREMY: So you could pick up this book if you weren’t a programmer or you haven’t done programming before?

3:03
GRAHAM HUTTON: Absolutely. So the book is aimed at basically anybody. So what it says in the preface is anybody over the age of 16 with a reasonable aptitude for scientific ideas could pick up a book like this and learn some Haskell from it. So it’s not necessarily aimed at professional programmers or students or laypeople. It’s aimed at basically anybody. So one half focuses on the basics. And it does it in this very systematic way, which I was talking about. So it covers the very basics of types and functions and list comprehensions and so on. And it builds up to more complicated things.

3:33
And then the second half of the book covers some of the more advanced topics that might be in, for example, a second course on functional programming like reasoning about programs, programming with monads, with applicative functors, with functors, foldables, traversables. All the kind of newer stuff in Haskell.

3:48
JEREMY: OK. Good. So a recommended read. Good. In Nottingham, you’ve got a functional programming lab.

3:55
GRAHAM HUTTON: Yes

3:56
JEREMY: And you have large team of people–

3:58
GRAHAM HUTTON: Yes.

3:58
JEREMY: –working with you there.

3:59
GRAHAM HUTTON: Yes.

4:00
JEREMY: What is it about Haskell that is particularly attractive in terms of research, a vehicle for doing interesting programming language experiments?

4:10
GRAHAM HUTTON: That’s a very good question. So I think for me, it’s the balance of being a very nice theoretical vehicle for studying programming language principles and kind of advancing that. But it’s also, on the other hand, a very practical language for writing real world applications. And I think that’s one of the things which is very exciting about the Haskell community. I mean it’s been going for about 30 years. And for the first maybe 15 or so years, it was very much an academic pursuit. And people like me were interested in writing academic papers about Haskell. And in the last 15 years and really in the last five years, things have really taken off.

4:41
And people who have no interest in this kind of language technology are now getting interested in it. And it’s become one of the hot languages that people want to learn. There’s quite a few jobs now. And we tell our students now that there’s a realistic chance if they want that they can get a job in Haskell afterwards. So it’s a really nice language to do research on because it’s both at the cutting edge of programming language research and also at the cutting edge of practice. I mean people in the real big players like the Facebooks, the Googles, the Microsofts, they’re writing real production software now using this cutting edge language technology.

5:12
So it’s a really nice research field to be in.

5:15
JEREMY: I want to pick up on something you mentioned there about the sense of community around Haskell and functional programming. I get the impression that there does seem to be more of a kind of gregarious and social nature to the Haskell community and online forums and news groups and so on. Have you picked up on that? And if so, why? Why is functional programming so friendly?

5:40
GRAHAM HUTTON: Absolutely. I mean that’s one of the reasons I became a functional programmer. My PhD wasn’t in that area. And I migrated to that afterwards. And it was partly because it was such a friendly community. I mean there’s lots of things going on, lots of forums, lots of newsgroups, meet ups, and things like that. It’s a really nice community to be part of. And I think the reason it’s developed like that is that it grew out from a core of about 20 or so people who started the Haskell language about 30 years ago. And they all shared this common vision. And they’re all very nice people as well.

6:11
And as the community has grown, they’ve wanted to keep this ethos, I think. So I think it’s that the people who started it were very nice and very, very clever people. And they wanted to keep that kind of nice community as a group.

6:23
JEREMY: That’s a good atmosphere. So people who use programming languages turn out like the people who invented the programming languages. [LAUGHTER] Very good. OK. Last question. This is about maybe a fun or interesting Haskell application that you’ve developed. Can you tell us something about a large piece of code that you’ve written in Haskell that’s just something cool?

6:46
GRAHAM HUTTON: So I think the one I’d pick up on is actually one that’s in the book, fortuitously enough, which is the countdown problem. And the idea is you’re given a collection of numbers, maybe six numbers. And you’re given the target number. And you need to build a simple arithmetic expression that evaluates to that target number. And I use this as an example in the book because it kind of illustrates many of the interesting things about functional programming. So what I do in the book is I start off with a kind of high level specification for what it means to solve this problem.

7:15
And then in a series of simple steps, I show how you can end up with an extremely efficient program that can solve any of these number games as soon as you basically hit the Return key. So I think, for me, this is a nice example of functional programming. Because it takes a problem which everyone is familiar with– a simple numbers game from the TV– and it shows how, in a very systematic way, you can write a very, very efficient program to solve it and end up with something quite interesting to look at and fun to look at. And I’ve been teaching that example to my first years for many years. And they always find this one quite engaging.

7:46
JEREMY: Yeah. That’s nice. So we’re saying Haskell is both fun and elegant.

7:50
GRAHAM HUTTON: Well, it’s fun, elegant, and practical. That’s why it’s such a nice field to be in at the moment.

7:55
JEREMY: Thanks, Graham.

7:56
GRAHAM HUTTON: Thanks very much, Jeremy.
```

[Professor Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/) from Nottingham University recently visited us at Glasgow. In fact, Graham got his [PhD](http://www.cs.nott.ac.uk/~pszgmh/bib.html#thesis) at Glasgow — many years ago. He runs a Functional Programming [research lab](http://fp.cs.nott.ac.uk/) in Nottingham, and he has recently released a [textbook about Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html).

During Graham’s visit, Jeremy took him to the university canteen, and quizzed him about his experiences of teaching and researching with the Haskell language.
