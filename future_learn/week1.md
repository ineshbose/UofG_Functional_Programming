# Week 1: Haskell First Steps

## Introduction

Introduction to the Haskell course

### Welcome to the Course
```
0:20
WIM VANDERBAUWHEDE: Hi and welcome to our Functional Programming in Haskell course. My name is Wim.

0:26
JEREMY SINGER: And I’m Jeremy.

0:28
WIM VANDERBAUWHEDE: We want to supercharge your coding. We will help you to be more effective by making you think about programming in a new way.

0:35
JEREMY SINGER: We are from the University of Glasgow, which was a prominent research contributor to the development of the Haskell programming language.

0:43
WIM VANDERBAUWHEDE: Admittedly, that was a long time ago in the late eighties.

0:47
JEREMY SINGER: When we were only students.

0:48
WIM VANDERBAUWHEDE: But Haskell has grown enormously in popularity. If you check the online programming chart, Haskell is consistently in the top 50, and it’s a cool topic of geek conversation. If you know Haskell, people will give you respect. Who knows? They might even give you a job.

1:05
JEREMY SINGER: In essence, though, this course is about more than just the intricacies of the Haskell programming language. We really want you to become familiar with the underlying concepts of functional programming. That’s a much broader aim. You see, functional concepts are popping up in all kinds of modern general purpose programming languages. Think about lambdas in C++ and Java, for instance.

1:29
WIM VANDERBAUWHEDE: Before we leave you to learn some Haskell, we would like to introduce you to the man after whom the Haskell language was named. Haskell Curry was a prominent logician in the 20th century, and the Haskell committee got the approval of his widow to use his first name for their great new language. I guess that just gives you an idea how deeply Haskell is rooted in formal logic.

1:52
JEREMY SINGER: Right. That’s enough talking. Let’s get coding. We’ve really enjoyed putting this course together, and we hope you’re going to enjoy it too. This week there are lots of interactive web-based programming exercises for you to get a grounding in the syntax and basic concepts of Haskell. Do try them out, and let us know how you get on in the comments section below. Happy coding!
```

**Welcome to Functional Programming in Haskell. During this six week course, you will learn how to develop simple programs in the Haskell language. We will also look at the origins and underlying philosophy of Haskell.**

More importantly, we will encourage you to think about software development in a *functional* way. This might be very different from your current approach to writing programs.

We ([Wim](https://www.futurelearn.com/profiles/2175260) and [Jeremy](https://www.futurelearn.com/profiles/1884921)) are academics at the [School of Computing Science](http://www.dcs.gla.ac.uk/), University of Glasgow. We do academic research in the areas of programming languages and systems.

## Haskell Basics: Expressions and Equations

In this activity we introduce basic Haskell syntax and the concepts of expressions and equations.

### Basic Elements By Example
```
0:08
WIM: Hello everyone. In this short lecture, I want to explain some basics of Haskell through examples from other languages that you might know. I will explain expressions, functions, types, and lists just through example without any deep detail. So first, expressions. In almost any programming language, you can write expressions such as, for example– and you can bind these to variables so that this expression value is contained in that variable. In Haskell, you can do the same thing, and what is more, in Haskell there are only expressions, whereas in most imperative languages there are statements and expressions. Haskell only has expressions. So the next basic element of Haskell is, of course, functions.

1:00
And we can compare functions in Haskell, for instance, with functions in Python. In Python you might write something like– so you define a function, hello, which takes an argument, name, and you return a string composed of the string, hello, and combined with name. So in Haskell you can do something quite similar, only a bit shorter.

1:42
So Haskell has very compact syntax where you don’t need parentheses to identify function arguments. You just use spaces. And the concatenation operator is separate from the addition operator.

2:13
The next basic element of Haskell is types. Types are very important in Haskell, and we will talk a lot about it, but for now we just want to give an example based on types in languages that you might know like C or Java. So for example, in C you could define a function like this.

2:42
So we have a function that takes two integers and returns an integer. So in Haskell we can write something similar, but the main difference is that the type declarations and the function definitions are separate. So we start with the type declaration. That would be– so the double colon indicates that a type declaration follows. And then we have the first argument is an integer. The second argument is an integer. The return value is an integer. Then the function definition.

3:20
So again, essentially the same thing but much more compact. So the final basic element of Haskell that I want to discuss is lists. In many languages, such as Ruby, JavaScript, and Python, you can write lists like this.

3:44
And, in fact, in Haskell this is exactly the same. So this is valid Haskell code. So lists are a very important component of the Haskell language. We’ll talk a lot more about them in future classes.
```

#### Expressions

In almost all programming languages you can create *expressions* such as:

```py
(b*b-4*a*c)/2*a
```

and you can assign these expressions to variables:

```py
v = (b*b-4*a*c)/2*a
```

In `Haskell`, you can do this as well, and what’s more: expressions are really all there is, there are no statements.

#### Functions

In `Python`, you can define a function such as

```py
def hello(name):
    return "Hello, "+name
```

In `Haskell` you can write this simply as:

```hs
hello name = "Hello, "++name
```

#### Types

`C` has *types*, for example:

```c
int f (int x, int y) {
    return x*y+x+y;
}
```

`Haskell` has much more powerful types than C, and we will talk a lot about types:

```hs
f :: Int -> Int -> Int
f x y = x*y+x+y
```

#### Lists

In many languages, e.g. Python, JavaScript, Ruby, ... you can create *lists* such as:

```py
lst = [ "A", "list", "of", "strings"]
```

`Haskell` also uses this syntax for lists.

To join lists, in `Python` you could write

```py
lst = [1,2] + [3,4]
```

In `Haskell` this would be very similar:

```hs
lst = [1,2] ++ [3,4]
```

#### Anonymous functions

In `JavaScript` you can define *anonymus* functions (functions without a name) such as:

```js
var f = function(x,y){return x*y+x+y};
```

In `Haskell`, such anonymous functions are called *lambda* functions and they are actually the basis of the language. Again, the syntax is very compact:

```hs
f = \x y -> x*y+x+y
```

#### Higher-order functions

Finally, in many languages, functions can operate on functions. For example, in `Perl` you can modify the elements in a list using:

```perl
map sub ($x){$x*2+1}, [1..10]
```

`Haskell` provides many of these so-called *higher-order functions*, and lets you define your own.

```hs
map (\x -> x*2+1) [1..10]
```

### Introduction to Expressions and Equations

#### Expressions

##### Haskell has no statements, only expressions!

- In an imperative language like C or Java,
    - there are *expressions* that denote small scale computations (`2*x`), and
    - *statements* that handle sequencing, looping, conditionals, and all the large scale operation of the program.
- *Pure functional programming languages don’t have any statements* — no assignments, no jumps.
- Instead, *all* computation is performed by evaluating expressions
- So, let’s start with expressions!
    - (We’ll still be working on expressions at the end of the course, since that’s all there is.)

##### Examples of integer expressions

**An expression evaluates to a result (usually written `e -w-> r` but we’ll use `e -- > r`). Haskell uses a similar notation for numbers and operators as most languages:**

```hs
2 -- > 2
3+4 -- > 7
3+4*5    {equivalent to 3+(4*5)} -- > 23
(3+4)*5   {equivalent to 7*5} -- > 35
```

##### Syntax of expressions

- Parentheses are used for grouping, just as in mathematics.
- If you don’t need parentheses for grouping, they are optional.
- Operators have precedence, e.g. `*` has "tighter" precedence than `+`, so `= 2 + 3 * 4` means `2 + (3 * 4)`.
- Use [the reference documentation](https://www.haskell.org/onlinereport/exps.html) for complete list of operators and their precedences, if you need them.

##### Function applications

- Expressions can contain function calls.
- A function takes argument(s), performs some computation, and produces result(s).
- The function *abs* gives the absolute value of a number.
- To use a function, you apply it to an argument. Write the function followed by the argument, separated by a space.

```hs
abs 5 -- > 5
abs (-6) -- > 6
```

##### Parentheses are for grouping

Good style

```hs
2+3*5
2+(3*5) -- might be clearer to some readers
abs 7
```

You don’t need parentheses. The following are legal, but they look silly:

```hs
(2) + ((3+(((((5)))))))
abs (5)
abs (((5)))
```

##### Functions with several arguments

- *min* and *max* are functions that take two arguments.
- The arguments are given after the function, separated by whitespace.
- Write `min 3 8`, don’t write `min(3, 8);`

```hs
min 3 8 -- > 3

max 3  8 -- > 8
```

##### Precedence of function application

- Function application binds tighter than anything else.
- So `f x + 3` means `(f x) + 3` and not `f (x+3)`
- If an argument to a function is an expression, you’ll need to put it in parentheses.

#### Equations

##### Equations give names to values

- Equations are used to give names to values.

```hs
answer = 42
```

- An equation in Haskell is a mathematical equation: it says that the left hand side and the right hand side denote the same value.
- The left hand side should be a name that you’re giving a value to.
- Correct: `x = 5*y`
- Incorrect: `2 * x = (3*x)**2` – Reassignment is not allowed in a pure FPL

##### Equations are not assignments

- A name *can be given only one value*.
- Names are often called "variables", but they *do not vary*.
- In Haskell *variables are constant*!

```hs
n = 1    -- just fine!
x = 3*n  -- fine
n = x    -- Wrong: can have only one definition of n
```

- Once you give a value to a name, you can never change it!
- This is part of the meaning of "pure" and "no side effects"

##### What about *n = n+1*?

- In imperative languages, we frequently say `n := n + 1`
- This is an assignment, not an equation!
- It means (1) compute the right hand side, using the old value of *n*; then (2) discard the old value of *n* and overwrite it with the new value.
- *There are no equations in imperative languages like C and Java.*
- In Haskell, it is valid to write *n = n + 1*.
    - This is an equation, not an assignment!
- It means: compute the value of *n* that has the property that *n = n + 1*.
- Haskell will try, and it will fail.

##### How can you compute without assignments?

- Think of an assignment statement as doing three things:
    1.  It evaluates the right hand side: computing a useful value.
    2.  It discards the value of the variable on the left hand side: destroying a value that might or might not be useful.
    3.  It saves the useful value of the RHS into the variable.
- In a pure functional language
    - We never destroy old values.
- We just compute new useful ones.
- If the old value was truly useless, the garbage collector will reclaim its storage.

#### Try Haskell!

##### Haskell in your browser

- You can try out Haskell in your browser! Go to [https://www.haskellmooc.co.uk](https://www.haskellmooc.co.uk/), our interactive Haskell environment, ready to use!

##### Installing Haskell

- You can install the Haskell compiler/interpreter on your own computer. Go to [https://www.haskell.org/platform](https://www.haskell.org/platform) to get the Haskell Platform for your system, it is very easy to install. For more details see [Installing Haskell for Yourself](https://www.futurelearn.com/courses/functional-programming-haskell/10/steps/1103579) in Week 2.
- *All software used in this course is free software.*
- Try experimenting with the expressions shown in this lecture.
- And try some experiments of your own.

##### The Haskell interpreter ghci

To launch the interactive Haskell interpreter *ghci*, just type `ghci` in your terminal:

```sh
[wim@fp4 ~]$ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude>
 -- Evaluate an expression --
Prelude> 3+4
7
```

To exit the Haskell interpreter, type `:quit` at the interactive prompt.

### Do it Yourself: Expressions, Functions and Equations

**The best way to learn a new programming language is to write some simple code by yourself. For the first two weeks of the course, we encourage you to use our online interactive Haskell environment.**

The initial tutorial will guide you through the basic concepts and we hope you will take some time to experiment. If you have questions or problems, please let us know in the comments.

Our interactive environment is an adaptation of the [tryhaskell](http://tryhaskell.org/) site by [Chris Done](http://chrisdone.com/). We are indebted to Chris for making his code [open source](https://github.com/chrisdone/tryhaskell).

#### Integer Expressions

We start with some simple integer arithmetic. At the prompt on the left there you can type in Haskell expressions.

Type an integer number, e.g. `42`, and observe that it evaluates to itself.

```sh
>> 42
42
:: Num a => a
```

OK, no surprises so far, you got back the number 42 as expected.

Now type a simple integer arithmetic operation, e.g. `6*7`, and observe that it evaluates to the expected result.

```sh
>> 6*7
42
:: Num a => a
```

#### Syntax of Expressions

You can use parenthesis to group subexpressions, e.g. `(3+4)*6`, but they are optional.

The arithmetic operations have the same precedence as in maths.

For example `3+4*6` means `3+(4*6)`.

```sh
>> 3+4*6
27
:: Num a => a
```

You can let Haskell prove this for you: try `3+(4*6) == 3+4*6`

```sh
>> 3+(4*6) == 3+4*6
True
:: Bool
```

So this expression returned True and it illustrates the use of the equality test operator.

You can nest as many parentheses as you like (even if it looks silly): `((6))*(((7)))`

```sh
>> ((6))*(((7)))
42
:: Num a => a
```

#### Special Cases

There are some special cases, in particular regarding the '-' sign. For example, try `4+-3`.

```sh
>> 4+-3
Not in scope: ‘+-’
Perhaps you meant one of these:
‘-’ (imported from Prelude), ‘++’ (imported from Prelude),
‘+’ (imported from Prelude)
```

As you can see, this fails: Haskell thinks you wanted to use a special operation '+-'.

Now, try `4+ -3` (that's right, just an extra space).

```sh
>> 4+ -3
Precedence parsing error
cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same infix expression
```

Again, that did not work as expected: Haskell does not allow you to combine 'infix' operations (like 3+4) with 'prefix' operations (like '-4').

So what should we do? Enclose the infix operation in parentheses: `4+(-3)`

```sh
>> 4+(-3)
1
:: Num a => a
```

And yes, that one worked! So in general it is best to enclose negative numbers with parentheses in expressions. Type `next` for the next lesson.

```sh
>> next
```

#### Functions

A function has a textual name (e.g. 'abs' or 'sin') rather than an operator.

It takes argument(s), performs some computation, and produces result(s).

To use a function in Haskell, you apply it to an argument: write the function followed by the argument, separated by a space.

For example try `abs 7`.

```sh
>> abs 7
7
:: Num a => a
```

As expected, applying abs to a positive number just returns its value

Now let's try a negative number: `abs (-3)`.

```sh
>> abs (-3)
3
:: Num a => a
```

And indeed, applying abs to a negative number returns the absolute value

Functions can take several arguments, e.g. min and max take two arguments. The arguments are given after the function, separated by whitespace.

For example, `min 3 8` or `max 3 8`.

```sh
>> min 3 8
3
:: (Num a, Ord a) => a
```

See? No need for parentheses!

To combine functions you need to know their precedence. In Haskell this is simple: Function application binds tighter than anything else. For example, try `sqrt 9+7`.

```sh
>> sqrt 9+7
10.0
:: Floating a => a
```

Surprised? Haskell interprets this as `(sqrt 9)+7`, not `sqrt (9+7)`.

So now try `sqrt (9+7)`.

```sh
>> sqrt (9+7)
4.0
:: Floating a => a
```

That worked as expected!

So if an argument to a function is an expression, you need to put it in parentheses.

So what about combining two functions? Try for example to apply 'min' to 'max 3 4' and '5'.

```sh
>> min max 3 4 5
Sorry, I can't display this result.
```

That doesn't work: Haskell thinks you're trying to apply 'min' to 4 arguments. You need parentheses around the inner function call, e.g. `min (max 3 4) 5`.
type `next` to continue to the next section

```sh
>> next
```

#### Equations

Equations are used to give names to values, e.g. `answer = 42`.

```sh
>> answer = 42
42
:: Num a => a
```

OK, you've defined an equation, now let's use it. Just type `answer`.

```sh
>> answer
42
:: Num a => a
```

Equations give names to values. So now 'answer' is just another name for '42'

An equation in Haskell is a mathematical equation: it says that the left hand side and the right hand side denote the same value.

The left hand side should be a name that you're giving a value to.

So now you can say `double = answer * 2`.

```sh
>> double = answer * 2
84
:: Num a => a
```

But you can't say `answer = answer * 2`!

```sh
>> answer = answer * 2
Evaluation killed!
```

If you tried it, you got an error message 'Evaluation killed!'

In a pure functional language like Haskell, you can't define a name in terms of itself.

And furthermore, you can only assign a name once!

For example, try `answer = 43`.

```sh
>> answer = 43
Conflicting definitions for ‘answer’
Bound at: <interactive>:1:6-11
<interactive>:1:20-25
```

As you can see, you get an error 'Conflicting definitions'

Reassignment is not allowed, variables are what is called 'immutable'.

This is a very important property because it means that you can always, anywhere in a program, replace a variable with its corresponding expression.

Please type `next` to continue to the recap page.

```sh
>> next
```

#### And that's the end of Tutorial 1.1!

Well done, you finished your first Haskell tutorial!.

Let's recap what we've learned:

1. Expressions: evaluate mathematical expressions, operators, rules of precedence and role of parenthese, infix and prefix operations.
2. Functions: calling existing functions, combining them and including them in expressions.
3. Equations: naming expressions using assignments, immutable variables.

### Test Your Understanding

#### Question 1

What is the difference between an expression and a statement in an imperative programming language

- [x]
    - Expressions denote small scale computations that return a value
    - Statements handle sequencing, looping, conditionals, and all the large scale operation of a program

- [ ] Expressions determine control flow and are often composed of statements

#### Question 2

What is the result of the following expression in Haskell, where `sqrt` returns the square root of its argument:

```hs
sqrt 16+9
```

- [ ] 5
- [x] 13

#### Question 3

What is the result of the following expression in Haskell, where `sqrt` returns the square root of its argument:

```hs
sqrt (16+9)
```

- [x] 5
- [ ] 13

#### Question 4

Give one reason why the following code is incorrect in Haskell:

```hs
x = 4
x = ((x * 2))
```

- [ ] Because there are too many parentheses
- [x] Because you can assign an expression to a name only once

#### Question 5

What is the result of the expression `abs -6` in Haskell, where `abs` returns the absolute value of its argument.

- [ ] 6
- [x] An error

#### Question 6


In Haskell, what is another appropriate name for this style of expression:

```hs
n = n + 1
```

- [x] An equation
- [ ] An assignment

#### Question 7

Is it valid to write `n = n + 1` in Haskell

- [x] Yes, it is a valid expression
- [ ] No, it is a syntax error

### Summary

- *Expressions* in Haskell are similar to those in other languages.
- There are only expressions in Haskell, i.e. no statements.
- Things that look like assignments are *not* updates of values but equations.

## Haskell Basics: Reduction, Functions and Lists

In this activity we explain how results are computed in Haskell, and the essential concepts of functions and lists.

### More Basic Elements by Example
```
0:07
WIM: Hello, everyone. In this short video, we’re going to look at some more basic elements of Haskell by examples from other languages. So we will look at anonymous functions, higher-order functions, blocks, and conditions. First, anonymous functions. So these are functions that don’t have a name and they occur in many languages. For example, in JavaScript. So in JavaScript you can write something like– so what we have here is a variable f that contains a complete function. In Haskell, these anonymous functions are called lambda functions and they’re really very important. They are actually the foundation of the language. So there is a very compact syntax for that. Again, in Haskell the same example would look as follows.

1:05
And again, we will talk a lot more about lambda functions in the next class. So another important element in many programming languages is the block structures that you use to define functions. For example, again in JavaScript we could define a function that returns the roots of a quadratic equation. So– we have a function that takes the coefficients of the quadratic equation and then we can compute the roots in the familiar way. So we have– so we have a function that defines a block of code where we find a number of variables. So first we have the square of the determinant. Then we take the square root to get to the determinant.

2:09
Then we compute the root and the first root of the quadratic equation and the second root and we return them both in a list. So, in Haskell, this same code looks very similar. And we will just define it by removing some of this syntax.

2:39
So we have a function of a, b, and c. And the function is actually defining a block using a let construct. So it says let and then a number of variables in the expression of this return. Because the whole thing is an expression, you don’t need the return statement.

3:00
Another very important construct in any programming language is a conditional construct. So an if-then, or if-then-else. For example, in Python we might define a function to compute the maximum of two numbers as follows.

3:24
So the function max takes two values, x and y, and we do a comparison. If x is greater than y, we return x. And otherwise, we return y. So nothing special there. Haskell has a very similar if-then statement, but again it’s an expression, not a statement. So let’s look at what it looks like.

3:50
And again, we need less syntax in Haskell than in Python.

3:57
So the last feature of Haskell that I want to illustrate is actually a little bit less basic– a little bit more advanced– it’s called higher-order functions. These are functions that operate on other functions, or functions that take functions as arguments. And again, we can compare this with other languages. In this case, we compare it with Perl. So, in Perl you could, for instance, compute the double of a list using the following code.

4:31
So what happens here is that we have an anonymous function that takes an argument and doubles it, and the map function makes this anonymous function work on the list from 1 to 10. So again, in Haskell, this is very similar.

4:53
Like that. So we have a map, which takes an anonymous function. So we have map, which takes an anonymous function and works on the list, and it will double every element in the list. So these higher-order functions are a lot more used in a functional language like Haskell than in an imperative language, but, as you can see, they do exist in other languages as well.
```

**A few more basic (and not-so-basic) elements of Haskell through comparison with other languages. We will not go into detail on the `Haskell` constructs, just show the similarities with constructs from languages you may know.**

#### Blocks

In `JavaScript` functions typically are blocks of code:

```js
function roots(a,b,c) {
    det2 = b*b-4*a*c;
    det  = sqrt(det2);
    rootp = (-b + det)/a/2;
    rootm = (-b - det)/a/2;
    return [rootm,rootp]
}
```

In `Haskell`, we would write this function as follows:

```hs
roots a b c =
    let
        det2 = b*b-4*a*c;
        det  = sqrt(det2);
        rootp = (-b + det)/a/2;
        rootm = (-b - det)/a/2;
    in
        [rootm,rootp]
```

Note that the `let ... in ...` construct is an *expression*, so it returns a value. That’s why there is no need for a `return` keyword.

#### Conditions

In `Python` we could write a function with a condition like this:

```py
def max(x,y):
    if x > y:
        return x
    else:
        return y
```

Of course `Haskell` also has an if-then construct:

```hs
max x y =
    if x > y
        then x
        else y
```

Again the `if ... then ... else ...` construct is an *expression*, so it returns a value.

#### Case statement

Many languages provide a `case` statement for conditions with more than two choices. For example, Ruby provides a `case` expression:

```rb
Red = 1
Blue = 2
Yellow = 3

color = set_color();
action = case color
    when Red then action1()
    when Blue then action2()
    when Yellow then action3()
end
```

In `Haskell`, the case works and looks similar:

```hs
data Color = Red | Blue | Yellow

color = set_color
action = case color of
    Red -> action1
    Blue -> action2
    Yellow -> action3
```

Note however how we use the type as the value to decide on the case, where in other languages we need to define some kind of enumeration.

#### Generics/Templates

In `Java` and `C++` there are generic data types (aka template types), such as:

```java
Map<String,Integer> set = new HashMap<String,Integer>();
```

In `Haskell`, you would write this as follows:

```hs
set :: Data.Map.Map String Integer
set = Data.Map.empty
```

The main difference is of course that `set` in `Haskell` is not an object but an immutable variable, so where in Java you would say:

```java
set.put("Answer",42)
```

In `Haskell` you would say:

```hs
set' = Data.Map.insert "Answer" 42 set
```

Because in `Haskell` variables are immutable, the return value of the `insert` call is bound to a new variable rather than updating the variable in place as in `Java`.

### Reduction, Functions and Lists

#### Reduction

##### A model of program execution

- A programmer needs a concrete model for how a program is executed.
- For imperative programs, we can execute statement by statement, keeping track of the values of variables (the stack) and where we are in the program (the program counter).
- Functional programs don’t have statements!
- The mechanism for executing functional programs is *reduction*.

##### Reduction

**Reduction is the process of converting an expression to a simpler form. Conceptually, an expression is reduced by simplifying one *reducible expression* (called "redex") at a time. Each step is called a reduction, and we’ll use `-- >` to show the result.**

```hs
  3 + (4*5)
--  >
  3 + 20
--  >
23
```

Reduction is important because it is the sole means of execution of a functional program. There are no statements, as in imperative languages; all computation is achieved purely by reducing expressions.

##### Unique reduction path

When a reduction is performed, there is only one possible answer. In this example, the computation has only one possible path:

```hs
  3 + (5 * (8-2))
--  >
  3 + (5 * 6)
--  >
  3 + 30
--  >
33
```

There is only one possible reduction path in that example, because in each step the current expression contains only one redex.

##### Multiple reduction paths

If an expression contains several redexes, there will be several reduction paths.

```hs
(3+4) * (15-9)
-- >
  7 * (15-9)
-- >
  7 * 6
-- >
 42
```

```hs
(3+4) * (15-9)
-- >
  (3+4) * 6
-- >
  7 * 6
-- >
  42
```

##### The result doesn’t depend on reduction path!

A fundamental theorem (the Church-Rosser theorem):

Every terminating reduction path gives the same result

This means that

- Correctness doesn’t depend on order of evaluation.
- The compiler (or programmer) can change the order freely to improve performance, without affecting the result.
- Different expressions can be evaluated in parallel, without affecting the result. *As a result, functional languages are leading contenders for programming future parallel systems.*

#### Functions

Haskell is a functional language so the function concept is essential to the language. A function takes one or more arguments and computes a result. Given the same arguments, the result will always be the same. This is similar to a mathematical function and it means that in Haskell there are no side-effects. There are two fundamental operations on functions: function definition (creating a function) and function application (using a function to compute a result).

##### Function definitions

- In Haskell, many functions are pre-defined in a standard library called the *prelude*.
- In due course, we’ll learn how to use many of these standard functions.

##### Defining a function

- But the essence of functional programming is defining your own functions to solve your problems!
- A function is defined by an *equation*.

```hs
f = \x -> x+1  -- lambda function
-- or
f x = x+1 -- named function
```

This is equivalent to in mathematical notation.

- The left hand side of the equation looks like a variable – and that’s what it is
- The right hand side is an expression that uses the local variables listed in parentheses and defines the result of the expression.

#### Function application

##### How function application works

- A function definition is an equation, e.g. `f = \x -> x + 1`
- The left hand side gives the name of the function;
- The right hand side (the "body") is an expression giving the formal parameters, and the value of the application. The expression may use the parameters.
- An application is an expression like `f 31`, where `31` is the argument.
- The application is evaluated by replacing it with the body of the function, where the formal parameters are replaced by the arguments.

##### Example of application

```hs
f  = \x - > x+1
  f 3
--  > {bind x=3}
  (x+1) where x=3
--  > {substitute 3 for x}
  3+1
--  >
4
```

#### Multiple arguments and results

##### Functions with several arguments

A function with three arguments:

```hs
add3nums = \x y z -> x + y + z
```

To use it,

```hs
10 + 4* add3nums 1 2 3
= {- put extra parentheses in to show structure -}
  10 + ( 4* (add3nums 1 2 3) )
  -- >
  10 + (4*(1+2+3) )
  -- >
  10 + (4*6)
  -- >
  10 + 24
  -- >
  34
```

#### Lists

##### A key datastructure: the list

- A list is a single value that contains several other values.
- Syntax: the elements are written in square parentheses, separated by commas.

```hs
['3', 'a']
[2.718, 50.0, -1.0]
```

##### Function returning several results

- Actually, a function can return only one result.
- However, lists allow you to package up several values into one object, which can be returned by a function.
- Here is a function (minmax) that returns both the smaller and the larger of two numbers:

```hs
minmax = \x y -> [min x y, max x y]
minmax 3 8  -- > [3,8]
minmax 8 3  -- > [3,8]
```

##### The elements are evaluated lazily

You can write a constant list

```hs
mylist = [2,4,6,8]
```

But the elements can be expressions. They are evaluated only when they are used. Suppose you define:

```hs
answer = 42
yourlist = [7, answer+1, 7*8]
```

Then

```hs
yourlist -- > [7, 43, 56]
```

But as long as you do not access the expression, it is not evaluated.

#### Constructing lists

##### Append: the `(++)` operator

- The `(++)` operator takes two existing lists, and gives you a new one containing all the elements.
- The operator is pronounced *append*, and written as two consecutive + characters.

```hs
[23, 29] ++ [48, 41, 44] -- > [23, 29, 48, 41, 44]
```

A couple of useful properties:

- The length of the result is always the sum of the lengths of the original lists.
- If *`xs`* is a list, then `[] ++ xs = xs = xs ++ []`.

##### Sequences

- Sometimes it’s useful to have a sequence of numbers.
- In standard mathematical notation, you can write `0,1,...,n`.
- Haskell has a sequence notation for lists.
- Write the sequence in square brackets, with start value, the operator `..`, and end value.
- `[0 .. 5] -- > [0,1,2,3,4,5]`
- `[100 .. 103] -- > [100,101,102,103]`
- The elements are incremented by 1

##### Sequences aren’t limited to numbers

- There are many *enumerable types* where there is a natural way to increment a value.
- You can use sequences on any such type.
- Characters are enumerable: there is a successor to each character.

For example:

```hs
[’a’ .. ’z’]
-- >
[’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’,’i’,’j’,’k’, ’l’,’m’,’n’,’o’,’p’,’q’,’r’,’s’,’t’,’u’,’v’, ’w’,’x’,’y’,’z’]
```

is a list of characters;

```hs
[’0’ .. ’9’]
-- >
[’0’,’1’,’2’,’3’,’4’,’5’,’6’,’7’,’8','9’]
```

is a list of characters (which happen to be the digit characters);

```hs
[0 .. 9]
-- >
[0,1,2,3,4,5,6,7,8,9]
```

is a list of numbers.

##### List comprehensions

- A list comprehension is a high level notation for specifying the computation of a list
- The compiler automatically transforms a list comprehensions into an expression using a family of basic functions that operate on lists
- List comprehensions were inspired by the mathematical notation *set comprehension*.
- Examples of set comprehensions:
    - A set obtained by multiplying the elements of another set by 3 is `{3 * x | x <- {1,...,10}}`.
    - The set of even numbers is `{2 * x | x <- N}`.
    - The set of odd numbers is `{2 * x + 1 | x <- N}`.
    - The cross product of two sets *A* and *B* is `{(a,b) | a <- A, b <- B}`.

##### Examples of list comprehensions

```hs
[3*x | x <- [1..10]]
-- >
[3,6,9,12,15,18,21,24,27,30]

[2*x | x <- [0..10]]
-- >
[0,2,4,6,8,10,12,14,16,18,20]

[2*x + 1 | x <- [0..10]]
-- >
[1,3,5,7,9,11,13,15,17,19,21]

[[a,b] | a <- [10,11,12] , b <- [20,21]]
-- >
[[10,20],[10,21],[11,20],[11,21],[12,20],[12,21]]
```

#### Operating on lists

##### Indexing a list

- We can index a list by numbering the elements, starting with 0.
- Thus a canonical form of a list with elements is .
- The operator takes a list and an index, and returns the corresponding element.

```hs
[5,3,8,7]  !! 2    -- > 8
[0 .. 100]  !! 81  -- > 81
['a'..'z'] !! 13  -- > 'n'
```

- If the index is negative, or too large, *undefined* is returned.
- For robust programming, we need to ensure either that all expressions are well defined, or else that all exceptions are caught and handled.
- Later, we’ll look at how to follow both of those approaches.

##### *head* and *tail*

- There are standard library functions to give the head of a list (its first element) or the tail (all the rest of the list)
- The result of applying *head* or *tail* to the empty list is *undefined*.

```hs
head :: [a] -> a
head [4,5,6] -- > 4
tail :: [a] -> [a]
tail [4,5,6] -- > [5,6]
```

- Recommendation: avoid using (head) and (tail), because you want to avoid undefined values so your programs are robust. Unless you’re doing something really sophisticated, you’re better off with pattern matching. There are, however, some cases where they are appropriate.

#### Lists are lazy

We have mentioned before that Haskell is "lazy", meaning that it only evaluates expressions when they are required for the evaluation of another expression.This behaviour extends to lists, so we can actually define infinite lists using sequences, for example `[1 .. ]` is the list of all positive integers. Another example is the `primes` function (from the Data.Numbers.Primes package) which returns an infinite list of prime numbers. A consequence of laziness in lists is that you can define lists containing very complex and time consuming expressions, and as long as you never access them they will not be evaluated. The same is true for an incorrect expression, for example defining `xs = [1,2,xs !! 5,4]` will not result in an error as long as you don’t access the third element.

Keep in mind that lists are also immutable. As a result, if you define `xs2 = xs ++ xs` and try to access the third element `xs2 !! 2` will still result in an error because `xs` has not been modified:

```hs
xs2 !! 2 -- > *** Exception: Prelude.(!!): index too large
```

Interestingly, if we change the definition of `xs` to `xs = [1,2,xs2 !! 5,4]`, then both `xs !! 2` and `xs2 !! 2` will return `2`:

```hs
xs = [1,2,xs2 !! 5,4]
xs2 = xs ++ xs
xs2 !! 2 -- > 2
xs !! 2 -- > 2
```

This is a consequence of Haskell’s expression evaluation through reduction: the order of the expressions does not matter.

### Do it Yourself: Functions and Lists

**Functions are the main way to abstract and encapsulate computation in Haskell. In this interactive tutorial, we will show you how to define your own functions. We will also explore functions without names, known as lambdas. Finally, we introduce the list data structure.**



### Test Your Understanding

#### Question 1

Does the evaluation order of Haskell sub-expressions affect the overall value of the expression?

- [x] No, evaluation order does not affect the final value.
- [ ] If expressions are evaluated in a different order, then the final value might be different.

#### Question 2

What is the difference between a named function and a lambda function assigned to a variable? For example:

```hs
f x = x + 1
-- or
f = \x -> x+1
```

- [x] There is no meaningful difference.
- [ ] It is less efficient to define functions in terms of lambdas.

#### Question 3

Given the definition:

```hs
sum_ratio = \x y z -> (x + y) / z
```
then what is the value of:

```hs
1 + 4* sum_ratio 4 2 3
```

- [ ] 5
- [x] 9

#### Question 4

What does the following expression evaluate to?

```hs
['a','d' .. 'z']
```

- [ ] "adz"
- [x] "adgjmpsvy"

#### Question 5


Given a list `xs`, what does the following expression evaluate to?

```hs
[]++xs == xs++[]
```

- [x] True
- [ ] False

### Summary

**Haskell programs compute by *reduction*, i.e. gradually replacing expressions by their values.**

A function takes one or more *arguments* and computes a *result*. Given the same arguments, the result will always be the same. In Haskell there are no side-effects.

The list is a key data structure. It is quite similar to lists in other languages. However note that Haskell lists are immutable.

## Finding Out More

Resources for new Haskell programmers

### Recommended Reading
```
0:08
JEREMY: Simon, you’ve got a big pile of books there. Haskell textbooks, to be precise. Would you like to give us some recommendations?

0:14
SIMON: Yeah I’ve got four here I was going to recommend. First is ‘Real World Haskell’. This was long-awaitedd. Actually it’s been out a while. It’s been out nearly 10 years. I guess they’re probably about to do the second edition. Came out in 2008. So it’s a brief introduction, somewhat, but they inquire a lot about using Haskell in practice so I think this is the one that the developers will often pick up. It’s for experienced programmers. In a similar way, but it says A Beginner’s Guide is ‘Learn You a Haskell’. An advantage of this is that it’s available free, online. And because it’s more recent, will talk about, in detail, about things like monads, monoids.

0:59
I suppose all the books talk about monads, but this goes into more detail about some of the more mathematical end of things. Graham Hutton’s book– one of the advantages is that this is nice and thin. So you’ll get it in 160 odd pages. I know Graham’s writing another edition of this. So by the time– this should be out early 2017, I would guess. So that’s about 10 years after this came out. But a good basic introduction. And I’ll conclude with mine.

1:29
JEREMY: [LAUGHS]

1:30
SIMON: Let’s have a shameless plug just for mine. I wrote the first edition of this around the late 90s.

1:35
JEREMY: What’s your book called?

1:37
SIMON: It’s called ‘Haskell: The Craft of Functional Programming’.

1:42
This edition is from 2011. I guess I will start thinking about doing another edition. It’s grown. It’s quite thick. I might try and take some material out of the new edition. But I think covers things in quite a lot of depth, quite a lot of examples that you can work through in there. But all of them have a different take so buy them all.

2:03
JEREMY: [LAUGHS] Thanks.
```

**Jeremy visited Simon Thompson at the University of Kent to find out about Haskell textbooks. Simon has a huge collection of programming language books.**

#### Book Recommendations

Simon mentions four textbooks in the video interview. Below are links to the websites for each book.

##### Real World Haskell

[Real World Haskell](http://book.realworldhaskell.org/) is a typical O’Reilly programming language book. The content is available [online for free](http://book.realworldhaskell.org/read/), along with reader comments.

##### Learn You a Haskell

[Learn You a Haskell](http://learnyouahaskell.com/) is a very informal introduction, but it does a great job of explaining complex concepts. You can [read it online](http://learnyouahaskell.com/chapters) for free. Some people [don’t like](http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=9049494&fileId=S095679681300004X) the author’s sense of humour, but I thought it was mostly ok.

##### Programming in Haskell

Simon mentioned that Graham Hutton is bringing out a 2nd edition of his textbook. Since the interview, [Programming in Haskell](http://www.cambridge.org/9781316626221) has been released. We highly recommend it as a simple and succinct introduction to Haskell programming.

Publishers Cambridge University Press have kindly set up a discount code that will allow FutureLearn Haskell learners to get 25% off the price of the new edition of _Programming in Haskell_ if they purchase from the [CUP website](http://www.cambridge.org/9781316626221). The discount code is **PIHMOOC** and this is simply entered in the _Subtotal and Discount Code_ section when checking out to get 25% off. This works on both the UK and USA sites from the publisher.

##### Haskell: the Craft of Functional Programming

Simon was slightly coy about [his own textbook](http://www.haskellcraft.com/craft3e/Home.html) in the interview. However we have found it very helpful while we were preparing this course. There are lots of copies of this book in Glasgow University library, so presumably our students find it helpful too!

#### Other Resources

[Haskell Programming from First Principles](http://haskellbook.com/) is a great textbook and increasingly popular in the Haskell community. Christopher Allen and Julie Moronuki are talented teachers of Haskell to novices. The book is content complete and will soon have the final and fully edited release, but you can purchase access to the ebook now. You get immediate access to the latest ‘version’ and updates as the book grows.

#### Online Help

We hope you find this course to be well-supported, in terms of the learning materials and the interactive comments. We have a team of tutors to support the learning community.

Other online sites also provide advice to Haskell beginners. For instance, the [Haskell Cafe](https://mail.haskell.org/mailman/listinfo/haskell-cafe) mailing list is a friendly place to post queries. There are also sub-reddits like [haskellquestions](https://www.reddit.com/r/haskellquestions).

### Spot the Difference

**You have spent some time this week exploring the Haskell language. Have you found Haskell to be different from other programming languages you have used?**

- How is Haskell different?
- Would you say that the ‘Haskell approach’ is better or worse than the other languages you know?

### End of Week 1
```
0:13
Hey! We’re at the end of the first week of our functional programming course. I hope you’ve enjoyed yourself. This week, we’ve looked at basic expressions and functions. We’ve studied a model of evaluation called reduction. And we’ve also introduced the list data type. If you’re a Haskell novice, please tell us what you found to be particularly interesting or challenging this week in the comments section below. Hope to see you back again next week. Thanks. Bye!
```

**That’s the end of the first week. We hope you have enjoyed your first steps in Haskell programming.**
