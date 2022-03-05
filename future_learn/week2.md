# Week 2: Haskell Building Blocks

## More Basic Haskell

Introducing booleans and lists

### Welcome to week 2

```
0:08
SPEAKER: Hi! We hope you enjoyed the first week of our functional programming course. This week, we’re going to be looking at more basic building blocks of the Haskell language. We’ll explore programs that have side effects. This is moderately complex in a pure language like Haskell. We’ll also be telling you how to install the GHC tool chain on your local machine so you won’t need to be logged into our online Haskell interpreter in order to run your Haskell programs. Let me leave you with a funny joke about Haskell from the XKCD webcomic. One person says, “Code written in Haskell is guaranteed to have no side effects.” And the other person quizzically replies, “Because no one will ever run it?”

1:01
We hope that you’ll all be running lots of Haskell code this week on the functional programming course.
```

**How are you getting on with functional programming? We introduced lots of basic concepts last week, and related many of them to traditional imperative languages. This week, we will continue to explore elementary Haskell features. We encourage you to try things out in the interactive exercise steps.**

Eventually, we want you to migrate from the online interpreter to using GHC — the Glasgow Haskell Compiler. We’ll walk you through the installation process at the end of this week. This will be important for later in the course, when your Haskell programs get more complex.

### Do it Yourself: Boolean Values and Expressions

**Relational operators test the relation between values, e.g. == tests whether two values are equal. These relational operators generally evaluate to boolean values.**

Another function that returns a boolean value is the list membership function called `elem`. In this interactive tutorial we will explore these constructs in simple Haskell code fragments. We encourage you to be creative with your coding!

#### Boolean Equality

Like many other languages, the double-equals operator == is used for testing value equality.

Type an integer equality test, e.g. `42==42`, and observe that it evaluates to True.

```sh
>> 42==42
True
:: Bool
```

OK, no surprises so far, you got back the truth value True as expected.

Now compare two different integer values for equality, e.g. `1 == 2`, and observe that the result is False.

```sh
>> 1 == 2
False
:: Bool
```

OK, no surprises so far, you got back the truth value False as expected.

#### The not-equals operator

Use the /= operator (it's supposed to look like an equals sign with a line through it), to test for inequality, e.g. `1 /= 2`.

```sh
>> 1 /= 2
True
:: Bool
```

You can apply these operations to other data types. Try comparing two Strings for equality, e.g. `"hello" == "hola"`.

```sh
>> "hello" == "hola"
False
:: Bool
```

Now try String inequality `"foo" /= "bar"`

```sh
>> "foo" /= "bar"
True
:: Bool
```

You can apply these operations to other data types. You might also try comparing two Bools directly, e.g. `True /= False`.

```sh
>> True /= False
True
:: Bool
```

So this expression returned True, illustrating the use of the equality test operator.

Now, what happens if you try to compare two values with different types? e.g. `True == 1`.

```sh
>> True == 1
No instance for (Num Bool) arising from the literal ‘1’
In the second argument of ‘(==)’, namely ‘1’
In the expression: True == 1
In the expression: let in True == 1
```

As you can see, this equality test fails: Haskell cannot compare two values that have *different types*. The full story is more complex, but for now, we can see that types *limit* the operations we can apply to particular values.

Haskell supports the standard comparison/relational operators, <,<=,>,>=. Try a simple comparison, e.g. `10 > 9`

```sh
>> 10 > 9
True
:: Bool
```

Note that relational operators also work on lists, in a dictionary-order manner (lexicographic). e.g. Try `[1,2,3] < [1,2,3,4]`

```sh
>> [1,2,3] < [1,2,3,4]
True
:: Bool
```

Since strings are lists of characters in Haskell, we can do the same kinds of comparison operations on strings. Check whether Aardvark comes before Aaronic in the dictionary with this code: `"Aardvark" < "Aaronic"`.

```sh
>> "Aardvark" < "Aaronic"
True
:: Bool
```

Now let's think about list membership. We want a boolean function that returns true if a value is part of a list, and false otherwise. This is the elem function. Try `elem 1 [1,2,3]`

```sh
>> elem 1 [1,2,3]
True
:: Bool
```

You see that element 1 is part of the list.

The elem function can be written infix, like an arithmetic operator, by enclosing its name in backquotes \`\`. Try `3 \`elem\` [1, 2, 3, 4, 5]`.

```sh
>> 3 `elem` [1, 2, 3, 4, 5]
True
:: Bool
```

In fact, Haskell permits any two-argument function to be written as an infix operator using backquote characters. For a further example, try the max function as an infix operator: `42 \`max\` 13`.

```sh
>> 42 `max` 13
42
:: (Num a, Ord a) => a
```

Also note that any Haskell infix operator, e.g. +, can be written as a prefix operator by enclosing it in parenthesis, like `(+) 1 1`

```sh
>> (+) 1 1
2
:: Num a => a
```

### Zip that List

```
0:05
JEREMY: Hello everyone. Time for some more functional programming. Let’s go. [closes jacket zipper]

0:13
You know, the zip is a remarkable invention. The two rows of interlocking teeth are combined using the zipper. Well today, we’re going to think about the Haskell zip function, which combines not interlocking teeth, but distinct lists of elements.

0:41
Here, I’m drawing a list of strings, different food stuffs here– fish, bread, and salt– represented as Haskell strings. Second list– chips, butter, and pepper. And what we see is that the first two elements from each list really belong together as a pair– fish and chips.

1:01
It’s the same with the second two elements from the lists. Bread and butter belong together as a pair.

1:10
And indeed the last element from each list, salt and pepper, they also belong together as a pair. So from a pair of lists, I now have a list of pairs, which is precisely the behavior of the zip function.

1:28
Hi. Now, we’re going to evaluate some zip function calls inside the Haskell interpreter, GHCi.

1:40
If you haven’t installed GHCi on your system yet, don’t worry. You can evaluate all the code I’m going to show you using the TryHaskell online environment, which you should have used for your tutorials. First, let’s zip together two lists of integers. We’ve got a list containing elements 1, 2, 3, and another list containing elements 4, 5, 6. And when I zip these two lists together, I’ll end up with a new list of pairs of integers. The first element from the first list is in a pair with the first element from the second list and so on.

2:24
I can zip together two lists of different types. There is an integer list. And here is a string, which, as you know, in Haskell is represented as a character list. So this should give me a list of pairs where the first element of each pair is an integer and the second element of each pair is a character. Indeed, 1 and a pair together. 2 and b are paired together. And 3 and c are paired together.

2:53
You might be asking, what if I want to zip together three lists to get a single list of triples? And that too is possible. Let’s use the zip3 function. So there is a character list, string Glasgow. String Beijing is another character list. And the string Nairobi is another character list. And indeed, when I evaluate this zip3 function, I’m going to end up with a single list of triples. The first triple has the first letter of each of those cities. And the last triple has the last letter of each of those triples and so on.

3:34
Conveniently, the strings Glasgow, Beijing, and Nairobi are all of seven characters in length. What happens if I try to zip together lists that have different number of elements? So here’s the list of integers from 1 to 10 inclusive, which has length 10. And here is the list of lowercase letters from A to Z inclusive, which has length 26. I’m going to try and zip together the list of integers 1 to 10 and the list of characters A to Z. Look, the output is as long as the shorter of the two inputs.

4:23
So I’ve produced a list of 10 pairs and used up all the input from the integers 1 to 10, but only the first elements from the alphabet A to Z. So the length of the output is the same as the length of the shorter of the two inputs.

4:46
Now, let’s think about the zipWith function, which is a generalization of zip. Whereas zip can only combine elements from two input lists by creating pairs of those elements, zipWith allows us to provide a function that tells us how to combine the elements from the input lists. So let’s do zipWith using the max function, which will allow us to combine input elements of integer lists.

5:18
And in the output list, we’ll have the maximum of the corresponding two entries in the input lists.

5:26
So you see the two integer lists there, 1, 2, 3 and 0, 2, 4. Well, the max element in each of the list locations will be in the output list when zipWith is evaluated here. And indeed, we see the first element 1 is the greater of the two corresponding elements in the input list. And for the last element, 4 is the greater of the two elements in the corresponding input lists.

5:57
Let’s try another zipWith. Recall that if we specify an infix operator in parentheses, it becomes a prefix function. So we’re going to combine elements from 1, 2, 3 and 0, 2, 4 by adding them together, which should give us the list of the sums. 1 and 0 makes 1. 2 add 2 makes 4. 3 add 4 makes 7.

6:26
Now, let’s replicate the behavior of the zip function we saw earlier using the more general zipWith function we know about now.

6:41
Remember, zip took an element from the first input list and then an element from the second input list and then produced in the corresponding element in the output list the pair of those two elements from the input list. What I’ve written is a lambda expression to perform this operation. Take one element, another element, then produce a pair of elements.

7:18
We’ll look at lambda expressions in more detail later in the course. But for now, don’t worry about the details. So let’s try this zipWith function here with this lambda expression using a pair the lists we looked at earlier. And we see that the output is the same list of pairs.
```

**The zip function is used to combine a pair of lists into a list of pairs. Since lists are such fundamental data structures in functional programming, it’s important to be able to manipulate them in flexible and powerful ways.**

Does `zip` remind you of similar functions in other languages? Perhaps `CONCAT` in SQL?

### Do it Yourself: Logical Thinking

[George Boole](https://en.wikipedia.org/wiki/George_Boole) introduced his laws of logic around 150 years ago. They form the basis for modern digital logic. In this interactive tutorial, we explore how to do calculations with boolean values in Haskell.

#### Boolean Negation

Boolean values are either True or False. True is the opposite of False, and vice versa. The not function returns the opposite boolean value, the logical complement. Try `not True`.

```sh
>> not True
False
:: Bool
```

You got back the boolean value False as expected.

Now perform a double negation, e.g. `not (not False)` and observe that the final result is the same as the initial value.

```sh
>> not (not False)
False
:: Bool
```

You got back the boolean value False as expected.

#### The And operator

Use the && infix operator as a boolean conjunction (AND function). This only evaluates to True when both its inputs are True i.e. `True && True`.

```sh
>> True && True
True
:: Bool
```

You got back the result True as expected.

Now try evaluating an AND expression where one of the inputs is False, e.g. `False && True`. What will the output value be?

```sh
>> False && True
False
:: Bool
```

You got back the result False as expected.

#### The Or operator

Boolean disjunction (logical OR) is the dual of the AND operation. In Haskell, use the infix || operator for OR. When at least one of the inputs is True, then the output of OR will be True. Try `True || False`.

```sh
>> True || False
True
:: Bool
```

You got back the result True as expected.

When both of the inputs are False, then the output of OR will be False. Try `False || False`.

```sh
>> False || False
False
:: Bool
```

You got back the result False as expected.

#### Exclusive OR

Haskell also defines the xor function, which returns true when its two boolean arguments are different (one is True and the other is False). Try `True \`xor\`False`. (Notice that we specify xor as an infix function with the backquotes here.)

```sh
>> True `xor` False
True
:: Bool
```

It's straightforward to enumerate the full truth table for two-input boolean functions. We could use a list comprehension expression to enumerate the input values: `[(x,y) | x<-[False, True], y<-[False, True]]`. Then we could map the boolean function over these input values (extracted from the pairs). For instance, here are the enumerated output values for the xor function: `map (\inputs -> xor (fst inputs) (snd inputs)) [(x,y) | x<-[False, True], y<-[False, True]]`.

```sh
>> map (\inputs -> xor (fst inputs) (snd inputs)) [(x,y) | x<-[False, True], y<-[False, True]]
[False,True,True,False]
:: [Bool]
```

#### Logic Operations with More Inputs

Sometimes, boolean logic functions like AND and OR have more than two inputs. Haskell supports these multi-input boolean operations with AND and OR functions that take a list of boolean values as a single input. Effectively, this is a fold of the && or || operator over the input list of boolean values.

Try `and [False, True, False, True]` or `or [True, True, False]`, for instance.

```sh
>> and [False, True, False, True]
False
:: Bool
```

You got back the result False as expected.

#### if Expressions

You might be used to `if` statements in imperative programming languages. Haskell has `if` expressions, which evaluate to either the `then` value or the `else` value, based on the `if` value.

Try `if 2*2==4 then "happy" else "sad"`.

```sh
>> if 2*2==4 then "happy" else "sad"
"happy"
:: [Char]
```

This if expression returned "happy", as you might expect.

The Haskell if expression is equivalent to the ?: ternary operator in C-like languages. The first subexpression (after the if) must have type Bool, then the subsequent two subexpressions (after then and else respectively) must have the same type as each other.

What happens if we supply a non-Boolean value for the first subexpression? Try `if 1 then 0 else -1`.

```sh
>> if 1 then 0 else -1
Could not deduce (Num Bool) arising from the literal ‘1’
from the context (Num a)
bound by the inferred type of it :: Num a => a at <interactive>:1:1
In the expression: 1
In the expression: if 1 then 0 else - 1
In the expression: let in if 1 then 0 else - 1
```

As you can see, this if expression fails to evaluate. Haskell tries to interpret the first subexpression as a Bool value, and fails.

There are other ways to have typing issues with if expressions, for instance when the then and else subexpressions have different types. Try `if False then 42 else "foo"`.

```sh
>> if False then 42 else "foo"
No instance for (Num [Char]) arising from the literal ‘42’
In the expression: 42
In the expression: if False then 42 else "foo"
In the expression: let in if False then 42 else "foo"
```

Once again, this if expression fails to evaluate because of type errors. Haskell detects that the then value and the else value have incompatible types, so it complains.

It is possible to have two values that are similar, i.e. they could be specialized to the same type, based on their type classes. For instance, try `if True then 42 else pi`.

```sh
>> if True then 42 else pi
42.0
:: Floating a => a
```

### Nothing but the Truth

Let’s review boolean values and operators we have introduced so far this week. We’ll also refresh our understanding of some list processing functions.

#### Question 1

What is the value of this expression?

```hs
let x = 5 in x == 5
```

- [x] `True`
- [ ] `False`
- [ ] `5`

#### Question 2

Which one of the following expressions should always evaluate to `True`?

- [ ] `"haskell" <> "python"`
- [x] `"haskell" < "python"`
- [ ] `haskell < python`

#### Question 3

What’s wrong with the following Haskell expression?

```hs
if (1) then "true" else "false"
```

- [ ] There should not be any brackets around the condition
- [ ] An `endif` statement is needed to show the end of the conditional expression
- [x] The condition is incorrectly typed, since it is an integer value

#### Question 4

Which one of the following expressions does not evaluate to 42?

- [ ] `(*) 21 2`
- [ ] `[7,23,42] !! ((+) 1 1)`
- [x] `head (zip [42] [True])`

#### Question 5

Given these definitions:

```hs
a = "england"
b = "scotland"
```

then which one of the following expressions has the greatest integer value?

- [ ] `length (zip a b)`
- [ ] `length (zip a a)`
- [x] `length (zip b b)`

## Input and Output

How to do text-based input and output in Haskell

### Why I/O?

```
0:08
JEREMY: How do computer programs interact with the outside world?

0:15
Using input and output. In this dissected PC hardware here, we see the input might arrive when the user presses keys on the keyboard or wiggles the mouse or, perhaps, when the file is read into memory from the disk. Outputs are visible to the external surroundings of the PC when the program writes data to the graphics card to be displayed on the screen or, perhaps, where a file is written back to disk. Haskell programs use something called the IO monad to interact with input/output. We’ll go into more detail about I/O and monads later on in the course. But for now, we just need to know two things.

1:11
Number one, when a function is using input or output, its type must contain IO. And number two, the IO monad ensures that input/output operations occur in a fixed sequence. For example, we must check the red button is pressed before we launch the missiles.

1:41
I’m going to invite you to look at the next interactive tutorial, now, which covers I/O in Haskell. Our I/O operations are much more mundane than launching missiles. We’ll just be writing strings to the console using putStrLn and reading character input from the standard terminal using getLine. As you do the tutorial, watch out and see where the IO type pops up. Thanks.
```

**Computers interact with the outside world via input and output (I/O). The Haskell programming language has specific support for I/O operations, which we will explore in the next few steps.**

### Do it Yourself: Input/Output

**In most programming languages, the first thing you learn is how to print strings to the console — think `PRINT` in Basic or `printf()` in C. However input and output in Haskell are moderately complex, due to the pure nature of the language.**

In this interactive tutorial, we demonstrate the basics of input (with `getLine`) and output (with `putStrLn`) however we are going to leave the details till later in the course. We also introduce the read and show functions for synthesizing values from Strings and vice versa.

#### Printing Strings

The Haskell function to print a character string to the terminal is called `putStrLn` (like `println` in Java or `print` in Python). Try printing a simple message like: `putStrLn "hello world"`

```sh
>> putStrLn "hello world"
hello world
:: IO ()
```

See how the string has been printed in the console window on the left?

You can concatenate several strings with the ++ operator, and print out one long string -- make sure you put brackets around the code so putStrLn prints the entire string. For instance, try `putStrLn ("good " ++ "morning" ++ " everyone")`.

```sh
>> putStrLn ("good " ++ "morning" ++ " everyone")
good morning everyone
:: IO ()
```

This concatenated string has also been printed in the console window on the left.

Now let's read in a character string from user input, this is like `sscanf` in C or input in Python. Call the `getLine` function, then type in some text at the > prompt followed by pressing the enter key

```sh
>> getLine
> hello world
"hello world"
:: IO String
```

See how the string is returned as a result of the function call in the console window on the left?

Now let's chain together some input and output - with appropriate sequencing. We want to find the name of a person, then print out a personalized greeting. `do { putStrLn "what is your name?"; x <- getLine; putStrLn ("hello " ++ x) }`. The do block sequences IO actions.

```sh
>> do { putStrLn "what is your name?"; x <- getLine; putStrLn ("hello " ++ x) }
what is your name?
> world
hello world
:: IO ()
```

As we have seen, you can sequence IO operations with the do construct. Values are bound to variables using the left arrow. We could read this as *x gets a value from getLine*. Be aware that you can't use the standard assignment (with the equals operator) for getLine, since it is an IO operation.
Once we have got a value from getLine, and bound it to a variable, then we can do standard function calls on this value, and bind it to another variable -- for instance, let's turn a name into upper case: `do { putStrLn "what is your name?"; n<-getLine; let nUpper = map toUpper n in putStrLn ("HELLO " ++ nUpper) }`

```sh
>> do { putStrLn "what is your name?"; n<-getLine; let nUpper = map toUpper n in putStrLn ("HELLO " ++ nUpper) }
what is your name?
> world
HELLO WORLD
:: IO ()
```

#### The Read function

It is possible to read values as strings, and convert them into other types. This is like the `atoi` function in C. Try `read "42" :: Int`. You need the `::Int` type annotation otherwise it is not clear what type of number the input String is meant to represent.

```sh
>> read "42" :: Int
42
:: Int
```

See how the string is converted to an `Int` value?

However 42 could also be a floating-point number, try `read "42"::Float`.

```sh
>> read "42"::Float
42.0
:: Float
```

Notice that the returned result has the `Float` type.

The show function is the dual of the read function. show takes a value and returns a String representation of that value. Try `show 42`.

```sh
>> show 42
"42"
:: String
```

Notice that the returned value has the String type. Only some types (those that derive Show) can be converted to Strings - see later

The show function allows arbitrary values to be printed. Try `putStrLn (show (6*7))`

```sh
>> putStrLn (show (6*7))
42
:: IO ()
```

In fact, there is a single function called `print` that does the composition of putStrLn and show .... try `print 42`.

```sh
>> print 42
42
:: IO ()
```

### I/O and a First Encounter with Monads

#### Pure Functions

So far, we have concentrated on *pure* functions. These kinds of functions take values as arguments, do some processing of those values, then return a result value. A pure function does not depend on the ‘state of the world’. The computation is entirely self-contained and independent. Given the same arguments, a pure function will *always* return the same result.

#### I/O is Impure

Input and output (I/O) operations are *impure*. They influence and interact with the ‘outside world’. Essentially, this is the only way to make computers do interesting things.

So far we have looked at the `getLine` function, which reads input from the user and returns it as a special kind of `String` value — an `IO String`. We have also used the `putStrLn` function, which takes a `String` input and prints it to the terminal, returning an empty IO value, i.e. `IO ()`.

The point of IO types is that we don’t want to mix up pure and impure functions — the type system keeps us honest. We know from a function’s type whether it is involved with I/O.

#### Sequencing Actions

Look at this simple function below.

```hs
let greet() = do 
    planet <- getLine
    home <- getLine
    putStrLn ("greetings " ++ planet ++ "ling.")
    putStrLn ("I am from " ++ home ++ ".")
    putStrLn "Take me to your leader."
```

In our tryhaskell web interpreter, you would need to enter it as a (very long!) one-liner:

```hs
do { planet <- getLine; home <- getLine; putStrLn ("greetings " ++ planet ++ "ling."); putStrLn ("I am from " ++ home ++ "."); putStrLn "Take me to your leader."}
```

Let’s try this out… copy and paste the one-liner into our [tryhaskell](https://www.haskellmooc.co.uk/) online system. **NB** *check there are no trailing space characters in your pasted expression — use backspace to remove them*. Press enter to evaluate the one-liner, then at the `>` prompt type `Earth` (enter) then `Mars` (enter). You should then see:

```
greetings Earthling.
I am from Mars.
Take me to your leader.
```

Notice that the ordering is important here:

1. we want the first `getLine` call to get the name of the planet we have landed on
2. we want the second `getLine` call to get the name of where we are from.

Order of function evaluation doesn’t matter in pure code - e.g.

```hs
let a = reverse "winston"
    b = reverse "churchill"
in "sir " ++ a ++ " " ++ b
```

Again, in [tryhaskell](https://www.haskellmooc.co.uk/), we should use a lengthy one-liner:

```hs
let a= reverse "winston"; b = reverse "churchill"  in "sir " ++a ++" "++ b
```

It doesn’t matter whether we do the first reverse before the second — the result of the expression is still the same. However this is *not* the case where I/O is concerned. Sequencing is vital for I/O actions.

One more thing to notice: inside the `do`, we associate results of IO functions with names using `<-`. Inside the `let`, we associate pure function results with names using `=`.

#### Monads are Hiding Below

The `do` notation allows us to *sequence* actions. This looks just like a sequence of commands in an imperative programming language. However, `do` is only syntactic sugar. Underneath, it is rewritten as a chain of function calls where the output of the first function becomes the input of the second function. The *bind* operator accomplishes this function sequencing. It is a key part of the IO monad. We are starting to scratch the surface of Haskell IO and discover it is highly complex. We will reserve discussion of Monads and bind operators for later in the course.

#### Summary

For now, all we need to understand is:

- I/O operations are impure
- use `do` to specify a sequence of actions
- use `<-` inside a `do` to associate input values with names
- any value or function that involves I/O has `IO` in its type
- a sequence of I/O actions is described as being *in the IO Monad*

## Installing GHC

How to install the Haskell Platform, a common platform for using and developing applications in Haskell.

### Installing Haskell for Yourself

**GHC stands for the ‘Glasgow Haskell Compiler’. This is actually a set of components that allow you to execute Haskell programs on your local machine.**
​
GHC works on Windows, Mac and Linux - although you will need to follow different instructions below for each operating system. Sadly, it does not currently run on mobile devices. There are alternative systems available, like [Raskell for iOS](https://itunes.apple.com/gb/app/raskell/id783015132).
​
The two bundled distributions of GHC are the [Haskell Platform](https://www.haskell.org/platform) and [Stack](https://docs.haskellstack.org/en/stable/README/). We give instructions for installing the Haskell Platform below, since this is what we use. However many people prefer Stack. Perhaps it’s best to consult the comments section to see what other learners are saying, then make your choice.
​

## Windows

​
Download the [installation program](https://www.haskell.org/platform/#windows) and follow the instructions. If you have a recent Windows version (7 or later) you will probably use the 64-bit Haskell. If you have an earlier Windows, you might need the 32-bit version.
​

## Mac OS X

​
Download the [installation program](https://www.haskell.org/platform/#osx) and follow the instructions. You can also use [MacPorts](https://www.haskell.org/platform/#osx-macports) or [build from source](https://ghc.haskell.org/trac/ghc/wiki/Building) if you are feeling more adventurous.
​

## Linux

​
Follow the [instructions](https://www.haskell.org/platform/#linux) for your distro. Most common distros have `haskell-platform` as a package in their package managers. You could also [build from source](https://ghc.haskell.org/trac/ghc/wiki/Building) if you are feeling more adventurous.
​

## How do I know it works?

​
You will need to open a terminal (cmd.exe on Windows, terminal on Mac OS, xterm on Linux) and type in `ghci` then press enter. If you see the prompt as in the screenshot above, then everything is ok. Type `:help` then press enter for an overview of the available commands; type `:quit` then press enter to finish ghci.

### How to Run GHCI

```
0:04
JEREMY: Hi. Now, you have installed the Haskell platform, let’s start using it. I’m in a terminal window here on my Mac OS laptop. You could do something similar on your Linux machine or maybe a DOS prompt in Windows. And I’m going to type in ‘ghci’, which is the interpreter for the Glasgow Haskell compiler. Press Enter. And look, it loads up. I’m running version 7.10 here. You might have a slightly different version, but hopefully, everything should work out fine. Notice that the prompt here, the interactive prompt, it says Prelude, which indicates that the standard Haskell library, Prelude, has been loaded. First of all, I’m going to type in a string– “hi everyone.” And this string value is returned for me.

0:56
I can see the type of ‘hi everyone.”

1:00
If I go to the start of the line and say ‘:type’ –

1:08
This is a GHCi command here– ‘:type’ “hi everyone” tells me that “hi everyone” is of type character list, which is equivalent to String, as you know. I can type in numbers, as well. Here’s 42. Arithmetic expression– here’s 1 plus 1. I can also use variables like x equals 3. And from now on, I can use x or use x inside arithmetic. Good. I could also call functions on strings. So let’s find out the length of “hi everyone.” We can also reverse “hi everyone,” which shows that that “enoyreve ih” is “hi everyone” written backwards. These standard functions, length and reverse, are defined for us in the Haskell Prelude.

2:06
Now, we’re going to define a function. Just inside the GHCi interactive prompt we can define the factorial function. Let fact n equal– remember this is the product of the first n positive numbers. If n==0, then fact of 0 is 1 else. We want to compute n times fact n minus 1. So if I say fact of 0, that should give us back 1. That’s the then case. If I say fact of 3, that gives us 3 times 2 times 1, which is 6. Fact of 5 is 120 and so on. So you see that we can define functions using let and then give the single line function definition here inside GHCi.

2:58
It might be the case that we actually want to define a function in a Haskell source code file. So let’s do that now. I’m going to start up the gedit text editor, and I’m going to create a new file. I’m going to save this file in the folder I’m in, which is– let’s find it quickly. I’m in the right folder now. And I’m going to call this file factorial.hs Haskell source code. So simple factorial definition– so I can say fact2– let’s give this a different name to the previous one– takes an integer and returns an integer. That’s the signature for the function.

3:56
Fact2 n equals– and I can use the same definition of n==0 then 1 else n times fact2 of n minus 1. Great. So let’s save this file– save. And then, we’ll go back to GHCi.

4:21
There we go– GHCi, :load factorial.hs.

4:29
And it loads. So you can use relative pathnames– relative to the directory that GHCi is running in– or you can use an absolute pathname, as well, if you have the full name of the directory where the file is stored. I’ve loaded the factorial file now. And it’s loaded OK without any problems. So now, I can say fact2 of 10, while it calculates the factorial. I can work up the type for fact2. It takes an integer argument and returns integer results.

5:02
Notice the ‘:t’ is shorthand for the type command. There we go– same again. And I could map fact2 over 1 to 10. And there we go. Now, let’s go and have another look at the definition of fact2.

5:24
We could actually rewrite this definition to make it a little bit neater.

5:37
Let’s try and save this and then :reload fact2– load factorial– it’s loaded again. And now, we can rerun the same command and get the same results. And perhaps, the function looks a bit more elegant now. What happens if I say fact2 of minus 1? Oh, sorry. I need to put minus 1 inside brackets. Otherwise, it’s trying to subtract 1 from the fact2 function. Let’s try this again. It starts running, and It runs and carries on running for a long time. This doesn’t match the first case because the input isn’t 0. So it matches the second case, which multiplies the number by one less than the number.

6:25
So we get minus 1 times minus 2 times minus 3 times minus 4, ad infinitum. We may get a stack overflow eventually, but I can’t afford to wait that long. So I’m going to press Control-C and then Enter. And hopefully, this should stop the computation.

6:46
To quit GHCi, I type in ‘:quit’, and I drop back into my command prompt. Let’s start again. I could also quit by typing Control-D. That gets me out, as well.

7:02
Let’s do some IO– putStrLn “hello.” It might be the case that I want to read in some input from the keyboard and then print out to the output. So I’m going to say, do. And x is getLine. Look, I get error message here, which says “The last statement in ‘do’ block must be an expression.” What I wanted to do there was write myself a multi-line do statement. But actually, I wasn’t able to do that. I need to say, set +m, in the GHCi environment to allow multi-line statements. Now, I can say, do x is getLine, line up the do commands. And now, I’m going to say, putStrLn “hello,” concatenated with x. Right.

8:08
I pressed Enter twice, and that got me out of the do construct. And now, GHCi is waiting for my input. So I’m just going to type in my name, Jeremy, Enter, and back comes “hello Jeremy.” So the do has been evaluated.

8:29
Let’s write ourselves one more function. I’m going to go back to get it. And I’m going to write myself a new function, which I’m going to call nobles.hs.

8:49
Make people noble– so mknoble is going to take a string parameter and return a string result. And I’m going to say mknoble. If I receive a name, then I’m going to return “Sir” plus that name ++ for concatenation. OK, let’s save this. And now, we should be able to load it into GHCi– load nobles. And now, I can say, mknoble “Alex Ferguson,” and “Sir Alex Ferguson” returns. That’s great– just go back here. I wonder what happens if I want to make a female person noble. Well, I really need another argument here, Boolean, that allows me to say, if– well, let’s give this parameter a name, female.

9:52
If female– sorry– then “Dame” else “Sir” ++ name. That’s all on one line. It just looks a bit messy. Perhaps I should move this along a bit so it’s on the next line. I need to indent it more than the if statement. OK, so now, I’m going to save that file and go and load it again. Colon l is short for load– nobles.hs. Now, I can mknoble. If I try and mknoble “Alex Ferguson,” it’s going to complain because I haven’t said whether he’s male or female. So look, it says now, “Couldn’t match expected type of ‘Bool’ with the actual type ‘Char’” because the first parameter should be a Boolean where I would have given it a string.

10:47
So we need to say mknoble False because “Alex Ferguson” is not a female. And now, it says, “Sir Alex Ferguson.” And then, I say mknoble True “Helen Mirren.” That becomes “Dame Helen Mirren.” Great. So that’s a function we defined here.

11:04
We can see the type of the function, :type mknoble, and now, it takes a Bool, then a string, and then returns a string. Great. Let’s quit. And I hope you have fun playing with GHCi. And again, if you have any problems or questions, let us know in the comments section below. Thanks.
```

**We used the online Haskell interpreter (tryhaskell) for the first few programming exercises. Now we want to move to GHCi, for some larger and more complex Haskell development.**

### Guessing Game

**Now let’s put together everything we have learned this week. We are going to write a moderately long Haskell program, consisting of multiple functions and I/O actions.**

The program is going to be a guessing game, called *Starman*. In this single-player, text-based game, there is a word which the player needs to guess. For each turn of the game, the player guesses a single letter. If that letter is correct, then the guessed letters are displayed in the correct places in the word. If that letter is incorrect, then the user loses a star. Once the user has no stars left, they have lost the game. However if the user guesses all the letters in the word, they have won the game.

Because this game is quite long, we should use a texteditor (like Notepad++ on Windows, TextEdit on Mac or Gedit on Linux). Well, I actually use emacs … if you’ve heard of it. Start by creating an empty text file called starman.hs — the `hs` extension is to indicate that this file contains Haskell source code.

#### Key Functions

The heart of the game involves checking the player’s guess. We want to know whether the guess was right. This outcome is a `Bool` value, either `True` or `False`. We need to update the displayed word, if the guess was right, by replacing appropriate dashes in the displayed word with the correctly guessed character. Therefore the result type of the function is a pair `(Bool, String)`. The first element of the pair is the guess outcome. The second element is the `String` to display to the user for the next round.

Now, the checking function needs to know:

- The secret word, a `String`
- The current display, also a `String`
- The character guessed by the player

These are the inputs to the checking function. So now we can state the type of the function:

```hs
check :: String -> String -> Char -> (Bool,String)
```

Here is a great *programming tip*. It’s always helpful to work out the *type* of a function first. This focuses your attention on what the function is supposed to compute, and what data it needs to do it. Good software engineers do *specification* before *implementation*.

What will the `check` function body look like? The player’s guess is correct if and only if the guessed character `c` is in the target word. So the guess is correct if

```hs
c `elem` word
```

The new displayed word will be:

```hs
[(if x==c then c else y) | (x,y) <- zip word display]
```

This is a list comprehension, where we select each letter from either the actual word or the old display. The word is plaintext, whereas the display starts with all dashed characters.

```hs
check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c
          then c
          else y | (x,y) <- zip word display])
```

The next function we will define is the `turn` function. This is the function that will be called each time it is the player’s turn to enter a guess. First we need to check how many guesses the player has left:

```hs
if n == 0
```

If there are any guesses left, then we need to see whether the player is correct or not:

```hs
if word == display
```

So we will have two `if` checks, each followed by `putStrLn` status messages and the end of the function calling sequence (since it is the end of the game). However if neither of the `if` conditions is true, then the player can take a turn, so we call another function to get another character from the user input.

```hs
turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn "You lose"
       else if word==display
              then putStrLn "You win!"
              else mkguess word display n
```

Note that there is a neater way to write the `turn` function, using Haskell guards, but we won’t learn about guards until next week.

```hs
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'
```

What is the type of `mkguess`? Can you work it out and add it before the function definition? We grab a line of user input, but only use the first character for the guess. This will fail if the user just hits *ENTER* without typing any characters, since `q` will be an empty string.

OK, so now we just need a top-level function. Let’s call this `starman`:

```hs
starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n
```

This function takes two arguments, the first is the word to be guessed, and the second is the number of incorrect guesses the player is allowed.

#### Running the Code

Let’s put all these four functions into a textfile, called `starman.hs`

Save the file, then start ghci perhaps by typing `ghci` into a DOS command prompt, running WinGHCi or typing `ghci` in a terminal window (macOS or Linux).

If you are in the correct directory, i.e. the one where you saved `starman.hs`, you should be able to type

```hs
:l starman.hs
```

and the program should load. It will either say something like:

```hs
[1 of 1] Compiling Main             ( starman.hs, interpreted )
Ok, modules loaded: Main.
```

or report an error if you have made a mistake in the source code anywhere. Check and make corrections if necessary. An error might look like this:

```hs
[1 of 1] Compiling Main             ( starman.hs, interpreted )
(some error report)
Failed, modules loaded: none.
```

The error report should have a line number, so you can see where the mistake is. Try to fix it by following the instructions, or comparing your code with what’s written above. Let us know in the comments section if you have any problems here.

When you get `Ok` from ghci, then you can run the program. At the ghci prompt type

```hs
starman "functionally" 5
```

and start playing the game! You will return to the GHCi prompt when the starman function completes.

We have provided the Haskell source code for `starman.hs` as a download below, along with some comments. You could use this, but it would be much better to type in the program yourself and try to understand it.

#### Possible Extensions

A real improvement to the game would be to generate a random word, perhaps from a list of words or a dictionary file. If you are feeling ambitious, you might try this. It would involve generating a random number i and read in the ith word from a dictionary. You might `import System.Random` and use a Haskell random number generator.

### What do you know about Haskell?

This quiz will review the basics of Haskell input and output, as well as your understanding of how to use ghci.

#### Question 1

What is wrong with this line of code, to return the number of characters typed by a user?

```hs
let x = getLine in length(x)
```

- [ ] nothing is wrong — the code should work fine
- [ ] Because `getLine` is a function, it needs arguments. There are no arguments given in this code.
- [x] The code associates the name `x` with the `getLine` function, rather than receiving a line of input from the user — and we can’t take length of a function.
- [ ] the code loops for ever

#### Question 2

What is the type of this function?

```hs
f name = putStrLn ("hello " ++ name)
```

- [x] `[Char] -> IO ()`
- [ ] `IO [Char]`
- [ ] `[Char] -> ()`

#### Question 3

How do you find the type of a defined function `f` in ghci?

- [ ] `:show f`
- [ ] `:load f`
- [x] `:type f`

#### Question 4

What is the difference between `->` and `<-` in Haskell syntax?

- [ ] <- indicates less than, whereas -> indicates greater than
- [x] <- is for associating names with values in do blocks whereas -> is used for defining functions.
- [ ] they are both two ways of representing the same thing.

#### Question 5

Why do you think the generation and use of pseudo-random numbers might occur inside a monad?

- [x] because the sequence of pseudo-random numbers is important, and the programmer needs to control it.
- [ ] because it is an interaction with ‘the outside world’
- [ ] because it is defined in the Haskell Prelude library

### End of Week 2
```
0:08
JEREMY: Here we are at the end of Week 2 of the functional programming course. This week, we thought about some more building blocks in the Haskell language. We looked at things like if statements and the Boolean data type. We have also had some more exploration of lists and how to manipulate list data structures. Then we moved on to considering monads, particularly getting user input from the keyboard and outputting characters to the terminal. Next week, we’re going to move on to more advanced constructs in Haskell. But for now, I want to leave you with an implementation challenge. Can you think of some more guessing games– like Guess the Number or Mastermind, if you’re familiar with that board game?

1:03
Post your ideas and links to your implementations in the comments section below. And I’ll have fun reading through those. I hope you have Haskell fun, too. And I’m looking forward to seeing you again next week when we’ll carry on with our functional programming adventure.
```

We now know enough Haskell to be dangerous. In this video, we give some suggestions for simple text-based games you might like to develop — probably adapting the Starman game we looked at this week.
