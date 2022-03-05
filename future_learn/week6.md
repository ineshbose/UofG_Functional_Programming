# Week 6: Think like a Functional Programmer

## Type Classes

In this activity we learn how to define our own type classes and we introduce the basic formalism underlying functional programming, the lambda calculus.

### Welcome to Week 6
```
0:08
WIM: Hello, and welcome to the sixth and final week of our Haskell Course. You have come a long way in these six weeks, from the very basics of Haskell, all the way to this week where we will discuss some more advanced topics. First, there’s type classes. In their 1990s article, the designers of Haskell said that certainly the type system is Haskell’s most innovative feature. And we think this is at least in part due to the type classes. And then there is monads, the mystery at the heart of Haskell. Now, what are they, and what is their relationship to type classes? Also, there’s the lambda calculus, the mathematical formalism that underlies functional programming.

0:52
It was invented by Alonzo Church in the 1930s to answer the question, what does it mean to do computation. All this and much more in this final week of our course. Enjoy.
```

**In this final week, we will cover some more advanced topics: type classes, the lambda calculus and monads. These are all concepts we have seen before, but we want to understand them in more depth.**

We also want to persuade you to continue using Haskell, after the course finishes. At the very least, now you know about functional programming — so you might develop code differently, whatever language you use.

### Types with Class
```
0:07
WIM: Hello, everyone. In this short video, we are going to do a quick recap of type classes that you’ve seen in the previous lectures. So type classes are a way to restrict the polymorphism in types so that you can actually overload functions and operators in specific cases. This is called ad-hoc polymorphism. So let’s give a few examples. So the first example is of the addition operation.

0:40
So the addition operation should work on any number types– should work on real types, on rational types, on integers. So we need some kind of type variable to express this. So we could write the signature as a type variable– so the operation takes something of type a– two elements and returns something of type a. But, of course, that’s not good enough because if a is not a number, it will not work. Similarly, if we have say smaller than– the less than operator– again, you could say, OK, it takes something of a certain type and something of the same type, and it returns a Boolean. So it indicates whether the comparison is true or false.

1:27
But again, that’s not good enough because, in general, types are not necessarily such that they can be compared to one another in a meaningful way. So in both cases, the way we solve this problem is by introducing type classes. So type classes are actually constructs that allow you to put constraints on the types without … you express that for instance, in this expression, a must be a number, and in this expression, a must be orderable. And the way we do that is we extend the syntax a little bit.

2:09
So for instance, we say for addition given that a belongs to the Num type class, meaning a is a number, then we can write this valid signature.

2:27
And the same for this one. Given that a belongs to Ord type class– that’s what this fat arrow means– then the following signature is valid.

2:46
So these are practical examples of type classes. Other ones that you have seen are the Show and the Read type classes. What we are going to do in the next step is to look at how you can define type classes– how type classes are defined and how you can make your own.
```

**Type classes are a way to overload functions or operators by putting constraints on polymorphism.**

For example, we have seen that:

```hs
(+) :: a -> a -> a
```

is not OK because we want to restrict addition to numbers.

Likewise,

```hs
(<) :: a -> a -> Bool
```

is not OK because it is not clear _a priori_ how to compare to arbitrary types.

To address this issue Haskell provides type classes. These restrict the polymorphism. For example:

```hs
(+) :: Num a => a -> a -> a
```

says that the type a must be a numeric type, and

```hs
(<) :: Ord a => a -> a -> Bool
```

says that a must be orderable.

### Type Classes in more Detail

#### Type class and instance declarations

##### Defining type classes

- A _type class_ is a set of types for which some operations are defined.
- Haskell has some standard type classes that are defined in the Standard Prelude.
- You can also define your own.

##### A type for bright colors

Suppose we’re computing with colors. Here’s a type, and a couple of functions.

```hs
    data Bright
      = Blue
      | Red
      deriving (Read, Show)

    darkBright :: Bright -> Bool
    darkBright Blue = True
    darkBright Red  = False

    lightenBright :: Bright -> Bright
    lightenBright Blue = Red
    lightenBright Red = Red
```

##### A type for milder colors

Now, suppose we have a different type that needs similar functions.

```hs
    data Pastel
      = Turquoise
      | Tan
      deriving (Read, Show)

    darkPastel :: Pastel -> Bool
    darkPastel Turquoise = True
    darkPastel Tan       = False

    lightenPastel :: Pastel -> Pastel
    lightenPastel Turquoise = Tan
    lightenPastel Tan       = Tan
```

##### Defining a type class

- Both of our color types have functions to decide whether it’s dark, or to lighten it.
- We can define a class and its corresponding functions.

```hs
    class Color a where
      dark :: a -> Bool
      lighten :: a -> a
```

This says

- is a type class
- The type variable stands for a particular type that is in the class
- For any type in , there are two functions you can use: and , with the specified types.

##### Defining instances for the type class

- An declaration says that a type is a member of a type class.
- When you declare an instance, you need to define the class functions.
- The following says that the type is in the class , and for that instance, the function is actually .

```hs
    instance Color Bright where
      dark = darkBright
      lighten = lightenBright
```

- Similarly, we can declare that is in , but it has different functions to implement the class operations.

```hs
    instance Color Pastel where
      dark = darkPastel
      lighten = lightenPastel
```

#### Predefined type classes

Haskell provides several standard type classes. We have a look at two of them: and .

##### The Num class

- is the class of numeric types.
- Here is (part of) its class declaration:

```hs
    class Num a where
      (+), (-), (*) :: a -> a -> a
```

###### Num instances

- There are many numeric types; two of them are and .
- There are primitive monomorphic functions that perform arithmetic on these types (these aren’t the real names):

```hs
addInt, subInt, mulInt :: Int -> Int -> Int
addDbl, subDbl, mulDbl :: Double -> Double -> Double

    instance Num Int where
      (+) = addInt
      (-) = subInt
      (*) = mulInt

    instance Num Double where
      (+) = addDbl
      (-) = subDbl
      (*) = mulDbl
```

###### Hierarchy of numeric classes

- There are some operations (addition) that are valid for all numeric types.
- There are some others (e.g. trigonometric functions) that are valid only for _some_ numeric types.
- Therefore there is a rich hierarchy of subclasses, including
  - class of numeric types that represent integer values, including , , and more.
  - class of types that can represent fractions.
  - class containing , , etc.
  - class of numeric types that have a minimal and maximal element.
  - class of types where you can access the representation as a sequence of bits, useful for systems programming and digital circuit design.
- If you want to get deeply into numeric classes and types, refer to [the Haskell documentation](https://www.haskell.org/onlinereport/basic.html).

##### The Show class

- We have been using to convert a data value to a string, which can then be written to output.
- Some values can be “shown”, but not all.
- For example, it is impossible in general to show a function.
- Therefore needs a type class!

###### Defining your own Show instance

```hs
    data Foo = Bar | Baz
```

We might want our own peculiar string representation:

```hs
    instance Show Foo where
      show Bar = "it is a bar"
      show Baz = "this is a baz"
```

Recall that when you enter an expression into ghci, it prints . So we can try out our strange instance declaration:

```sh
    *Main> Bar
    it is a bar
    *Main> Baz
    this is a baz
```

###### Deriving Show

This is a similar type, but it has a clause.

```hs
    data Foo2 = Bar2 | Baz2
      deriving (Read, Show)
```

Haskell will automatically define an instance of for , using the obvious definition:

```sh
    *Main> Bar2
    Bar2
    *Main> Baz2
    Baz2
```

##### More standard typeclasses

Here is a summary of some of the type classes defined in the standard libraries.

- numbers, with many subclasses for specific kinds of number.
- types that can be “read in from” a string.
- types that can be “shown to” a string.
- types for which the equality operator is defined.
- types for which you can do comparisons like , , etc.
- types where the values can be enumerated in sequence; this is used for example in the notation and .

```sh
    *Main> [1..10]
    [1,2,3,4,5,6,7,8,9,10]
    *Main> ['a'..'z']
    "abcdefghijklmnopqrstuvwxyz"
```

### Summary

**Type classes provide a set of function type signatures. Haskell has special keywords to create type classes. To create an instance, we create functions for a given type that match the signature of the type class.**

## Geek Greek

We introduce the ideas of lambda calculus and show how it fits into a historical context

### Introduction to the Lambda calculus

- The lambda calculus was developed in the 1930s by Alonzo Church (1903–1995), one of the leading developers of mathematical logic.
- The lambda calculus was an attempt to formalise functions as a means of computing.

#### Significance to computability theory

- A major (really _the_ major) breakthrough in computability theory was the proof that the lambda calculus and the Turing machine have exactly the same computational power.
- This led to _Church’s thesis_ — that the set of functions that are effectively computable are exactly the set computable by the Turing machine or the lambda calculus.
- The thesis was strengthened when several other mathematical computing systems (Post Correspondence Problem, and others) were also proved equivalent to lambda calculus.
- The point is that the set of effectively computable functions seems to be a fundamental reality, not just a quirk of how the {Turing machine, lambda calculus} was defined.

#### Significance to programming languages

- The lambda calculus has turned out to capture two aspects of a function:
  - A mathematical object (set of ordered pairs from domain and range), and
  - An abstract black box machine that takes an input and produces an output.
- The lambda calculus is fundamental to denotational semantics, the mathematical theory of what computer programs mean.
- Functional programming languages were developed with the explicit goal of turning lambda calculus into a practical programming language.
- The ghc Haskell compiler operates by (1) desugaring the source program, (2) transforming the program into a version of lambda calculus called _System F_, and (3) translating the System F to machine language using _graph reduction_.

#### Abstract syntax of lambda calculus

- We will work with the basic lambda calculus “enriched” with some constants and primitive functions (strictly speaking, that is not necessary).
- The language has constants, variables, applications, and functions.

```hs
exp
    = const
    | var
    | exp exp
    | \ var -> exp
```

#### Variables

- Each occurrence of a variable in an expression is either _bound_ or _free_
  - In , the occurrence of in is _bound_ by the .
  - In , the occurrence or is _free_. It must be defined somewhere else, perhaps as a global definition.
- In general, an occurrence of a variable is bound if there is some enclosing lambda expression that binds it; if there is no lambda binding, then the occurrence if free.

We need to be careful: the first occurrence of is free but the second occurrence is bound.

```hs
a + (\ a -> 2^a) 3  -- >   a + 2^3
```

Being free or bound is a property of an _occurrence_ of a variable, not of the variable itself!

#### Conversion rules

- Computing in the lambda calculus is performed using three _conversion rules_.
- The conversion rules allow you to replace an expression by another (“equal”) one.
- Some conversions simplify an expression; these are called _reductions_.

#### Alpha conversion

- Alpha conversion lets you change the name of a function parameter consistently.
- But you can’t change free variables with alpha conversion!
- The detailed definition of alpha conversion is a bit tricky, because you have to be careful to be consistent and avoid “name capture”. We won’t worry about the details right now.

```hs
(\x -> x+1) 3
(\y -> y+1) 3
```

#### Beta conversion

- Beta conversion is the “workhorse” of lambda calculus: it defines how functions work.
- To apply a lambda expression an argument, you take the body of the function, and replace each bound occurrence of the variable with the argument.

```hs
(\x -> exp1) exp2
```

is evaluated as

Example:

```hs
(\x -> 2*x + g x) 42
```

is evaluated as

#### Eta conversion

- Eta conversion says that a function is equivalent to a lambda expression that takes an argument and applies the function to the argument.

```hs
(\x -> f x)
```

is equivalent to

Example (recall that is a function that multiplies its argument by 3)

```hs
(\x -> (*3) x)
```

is equivalent to

Try applying both of these to 50:

```hs
(\x -> (*3) x) 50
```

is the same as

#### Removing a common trailing argument

There is a common usage of Eta conversion. Suppose we have a definition like this:

```hs
f x y = g y
```

This can be rewritten as follows:

```hs
f = \x -> (\y -> g y)
f = \x -> g = f x = g
```

Thus the following two definitions are equivalent:

```hs
f x y = g y
f x = g
```

In effect, since the last argument on both sides of the equation is the same (), it can be “factored out”.

### There are Only Functions! (Optional)
```
0:05
WIM: Hello, everyone. In this optional video, I want to explain that, really, in functional languages there are only functions. What I want to do is give you a quick intuition how we can remove all the syntactic sugar from a functional language and reduce it to only lambda functions. So the full elaboration of how you can do this is explained in the article on the site. We will just cover the basic ideas. So let’s start with the let construct. Suppose we have something like– so we a let block, where we have n is assigned to 10, and we define the function f of x being x plus 1. And we apply f to n.

1:02
So basically the whole thing should return 11. So, we want to rewrite this so that there will only be lambda functions left. So the obvious first rewrite we can do is rewrite this function definition of the lambda. So, very simply.

1:26
OK. So then the next thing we will do is, we have a let that contains two variables, and we will essentially do the equivalent of currying in the let blocks. So we will transform this let block in a nest of let blocks. So what we’re doing is–

2:05
OK. So now we have a nested let. Now we have this let expression, with just a single variable here, and we can rewrite this as a lambda expression as follows. So I’ll just put it on this side here.

2:32
So this whole let block here is actually entirely equivalent to a lambda function applied to the function that defines f. So we bind f to this function, and apply it to n. So all we have to do is do the same for other let. And that’s quite simple. So we take the other let and transform it into a lambda expression, which says, basically, we have n applied to this construct here. And apply to 10. So if we just straighten it out a bit, we get–

3:22
So we have transformed the whole nested let into a series of lambdas. This you can see, it’s entirely equivalent, but it’s a lot less easy to type, which is why the let syntax is available in the language of course.

3:42
So the next thing we want to do is see if we can define conditional expressions, and so on. And for that we need to define the values of true and false. You might wonder, how can we define the values of true and false without having any numbers, because remember we’re going to use only functions, so even numbers are not defined yet. So it’s actually quite easy. We define true and false as functions. So we say true is defined as a function of two elements, which returns the first element. And false is a function of two elements which returns the second elements. And that’s all there is to it.

4:25
This is our– this is by definition, in our system, true and false. And now we can use these values of true and false, these definitions to define things like if then else constructs, or– Let’s do that next. So, in if then else– so that in Haskell it looks if condition then if true else if false. So condition, if true and if false are expressions in their own right, condition evaluates to true or false. So the first thing is this if then else, we know it’s an expression. And it’s actually just syntactic sugar for a function, and we will call this function if then else. So we have if then else condition if true if false.

5:25
So this is our first step in removing some syntactic sugar. So now with these definitions for true and false, it turns out that the definition of this if then else is actually quite simple. It is very simply– because remember, condition evaluates to true or false. So it must return one of these two functions. These are functions of two elements, that return either the first or the second. So if it’s this one, then condition will return this. If that one, condition will return that. So this way, we have defined if then else.

6:08
Neat, huh?

6:22
Finally, I want to show how to define lists. So let’s say we have a list which is just 1, 2, 3. We already know from what we’ve seen earlier that this bracket syntax for list is actually syntactic sugar for a combination of the cons operation and the empty list. So we know that this is entirely equivalent to saying 1, 2, 3 empty list. Or written out more explicitly using the cons function.

7:06
So what we have to do now is define cons and the empty list using lambda functions and nothing else. And we will use the same trick again of using– defining a function that returns a function, because we have nothing else, remember? So we define cons as a lambda function of x and xs, which returns, in its own right, a function that operates on x and xs.

7:42
So this is– because we don’t want any syntactic sugar, so normally cons is of course a function of x and y. We’ve rewritten it as a variable binding to a lambda expression. And this lambda expression in x and xs returns this particular function. So all we need now is the empty list. And this is again very simple. So we define the empty list as a function of anything to true. And the reason for this, why it has to be true, is because we will work on from this and define things like test for emptiness of the list, and length of the list, and so on. And with this definition the whole system is consistent.

8:32
So in the same fashion you can define tuples and then you can go on to define list operations, recursions, folds, maps. And then you can go on to define numbers simply by saying that you have a 0, of some arbitrary starting point, and then a function that always defines the next number, and so on. All that is explained in the article in more detail. But in this way, we can define our complete language in terms of nothing else than lambda functions. So really, there are only functions. OK.
```

#### In a Functional Language, There are Only Functions

Although it might seems that a language like Haskell has a lot of different objects and constructs, we can express all of them in terms of functions. Watch the video to get a little insight, then read on to find out more …

##### Variables and `let`

```hs
    let
      n = 10
      f x = x+1
    in
      f n

-- One variable per let =>

    let
      n = 10
    in
      let
        f x = x+1
      in
        f n

-- Rewrite f as lambda =>

    let
      n = 10
    in
      let
        f = \x -> x+1
      in
        f n


-- Rewrite inner let as lambda =>

    let
      n = 10
    in
      (\f -> f n) (\x -> x+1)

-- Rewrite outer let as lambda =>

    ( \n -> ((\f -> f n) ( \x -> x+1 )) ) 10
```

So variables and let expressions are just syntactic sugar for lambda expressions.

##### Tuples

```hs
    tp = (1,"2",[3])
```

The tuple notation is syntactic sugar for a function application:

```hs
    tp = mkTup 1 "2" [3]
```

The tuple construction function can again be defined purely using lambdas:

```hs
    mkTup = \x y z -> \t -> t x y z
```

The same goes for the tuple accessor functions:

```hs
    fst tp = tp (\x y z -> x)
    snd tp = tp (\x y z -> y)
```

##### Lists

Lists can be defined in terms of the empty lists `[]` and the `cons` operation `(:)`.

```hs
    ls = [1,2,3]

Rewrite using : and [] =>

    ls = 1 : 2 : 3 : []
```

Or using cons =>

```hs
    ls = cons 1 (cons 2 (cons 3 []))
```

###### Defining `cons`

We can define `cons` using only lambda functions as

```hs
  cons = \x xs ->
   \c -> c x xs
```

Thus

```hs
    ls = cons 1 (...)
       = \c -> c 1 (...)
```

We can also define `head` and `tail` using only lambdas:

```hs
    head ls = ls (\x y -> x)
    tail ls = ls (\x y -> y)
```

###### The empty list

We can define the empty list as follows:

```hs
    [] = \f -> true
```

The definitions for `true` and `false` are given below under Booleans.

Then we can check if a list is empty or not:

```hs
    isEmpty lst = lst (\x xs -> false)
```

A non-emptylist is always defined as:

```hs
    lst = x:xs
```

which with our defintion of `(:)` is

```hs
    lst = (\x xs -> \c -> c x xs) x xs
    = \c -> c x xs
```

Thus,

```hs
    isEmpty lst
    = isEmpty (\c -> c x xs)
    =  (\c -> c x xs) (\x xs -> false)
    = false

    isEmpty []
    = isEmpty (\f -> true)
    = (\f->true) (\x xs -> false)
    = true
```

###### Recursion on lists

Now that we can test for the empty list we can define recursions on lists such as `foldl`, `map` etc.:

```hs
    foldl f acc lst =
      if isEmpty lst
        then acc
        else foldl f (f (head lst) acc) (tail lst)
```

and

```hs
    map f lst  =
      let
        map' f lst lst' = if isEmpty lst then (reverse lst') else map' f (tail lst) (head lst: lst')
      in
        map' f lst []
```

with

```hs
    reverse lst = (foldl (\acc elt -> (elt:acc)) [] lst
```

The definitions of `foldl` and `map` use an if-then-else expression which is defined below under Conditionals.

######  List concatenation

```hs
    (++) lst1 lst2 = foldl (\acc elt -> (elt:acc)) lst2 (reverse lst1)
```

######  The length of a list

To compute the length of a list we need integers, they are defined below.

```hs
    length lst = foldl calc_length 0 lst
      where
        calc_length _ len = inc len
```

##### Conditionals

We have used conditionals in the above expressions:

```hs
    if cond then if_true_exp else if_false_exp
```

Here `cond` is an expression returning either `true` or `false`, these are defined below.

We can write the if-then-else clause as a pure function:

```hs
    ifthenelse cond if_true_exp if_false_exp
```

##### Booleans

To evaluate the condition we need to define booleans:

```hs
    true = \x y -> x
    false = \x y -> y
```

With this definition, the if-then-else becomes simply

```hs
    ifthenelse cond if_true_exp if_false_exp = cond if_true_exp if_false_exp
```

###### Basic Boolean operations: `and`, `or` and `not`

Using `ifthenelse` we can define `and`, `or` and `not`:

```hs
    and x y = ifthenelse x (ifthenelse y true) false
    or x y = ifthenelse x true (ifthenelse y true false)
    not x = ifthenelse x false true
```

###### Boolean equality: `xnor`

We note that to test equality of Booleans we can use `xnor`, and we can of course define `xor` in terms of `and`, `or` and `not`:

```hs
    xor x y = (x or y) and (not x or not y)
```

```hs
    xnor x y = not (xor x y)
```

##### Signed Integers

We define an integer as a list of booleans, in thermometer encoding, and with the following definitions:

We define usigned 0 as a 1-element list containing `false`. To get signed integers we simply define the first bit of the list as the sign bit. We define unsigned and signed versions of `0`:

```hs
    u0 = false:[]
    0 = +0 = true:u0
    -0 = false:u0
```

For convenience we define also:

```hs
    isPos n = head n
    isNeg n = not (head n)
    isZero n = not (head (tail n))
    sign n = head n
```

###### Integer equality

The definition of `0` makes the integer equality `(==)` easier:

```hs
    (==) n1 n2 = let
      s1 = head n1
      s2 = head n2
      b1 = head (tail n1)
      b2 = head (tail n2)
      if (xnor s1 s2) then
        if (and (not b1) (not b2))
          then true
          else
            if (and b1 b2)
              then  (==) (s1:(tail n1)) (s2:(tail n2))
              else false
        else false
```

###### Negation

We can also easily define negation:

```hs
    neg n = (not (head n)):(tail n)
```

###### Increment and Decrement

For convenience we define also define increment and decrement operations:

```hs
    inc n = if isPos n then true:true:(tail n) else if isZero n then 1 else false:(tail (tail n))
    dec n = if isZero n then -1 else if isNeg n then false:true:(tail n) n else true:(tail (tail n))
```

###### Addition and Subtraction

General addition is quite easy:

```hs
    add n1 n2 = foldl add_if_true n1 n2
      where
        add_if_true elt n1 = if elt then true:n1 else n1
```

In the same way, subtraction is also straightforward:

```hs
    sub n1 n2 = foldl sub_if_true n1 n2
      where
        sub_if_true elt n1 = of elt then (tail n1) else n1
```

###### Multiplication

An easy way to define multiplication is by defining the `replicate` and `sum` operations:

```hs
    replicate n m =
      let
        repl n m lst = if n==0 then lst else repl (dec n) m (m:lst)
      in
        repl n m []

    sum lst = foldl add 0 lst
```

Then multiplication simply becomes

```hs
    mult n m = sum (replicate n m)
```

In a similar way integer division and modulo can be defined.

##### Floats, Characters and Strings

We note that floats and chars use an integer representation, and strings are simply lists of chars. So we now have a language that can manipulate lists and tuples of integers, floats, chars and strings.

### We Love Lambda!

#### Question 1

What is the result of evaluation of the following expression?

```hs
(\x y -> x*x-y*y) 3 4
```

The result is `-7`

#### Question 2

What is the result of evaluating the following expression?

```hs
map (\x -> length x) ["This","is", "a","test"]
```

- [ ] an error
- [ ] 4
- [x] [4,2,1,4]

#### Question 3

What is the result of evaluating the following expression?

```hs
(\x -> (\y -> y x)) "x" (\y -> y)
```

The result is: `"x"`

#### Question 4

What is the result of evaluating the following expression?

```hs
(\x -> (\y -> x y)) "x"
```

- [ ] a partially applied function
- [x] a type error
- [ ] a string value

#### Question 5

What is the result of evaluating the following expression?

```hs
(\x f -> f x) 4 (\x -> x*x)
```

- [ ] 4
- [x] 16
- [ ] a partially applied function

#### Question 6

What is the result of evaluating the following expression?

```hs
(\x -> 1) 2
```

- [x] 1
- [ ] 2
- [ ] a type error

### Summary

**The lambda calculus is a formal model for computation using only lambda functions. Haskell, like other functional languages, is based on the lambda calculus.**

The reduction rules from the lambda calculus are used to reduce expressions in Haskell.

## The M-word

Introducing monadic computations

### We Already Know About Monads
```
0:04
JEREMY: Functional programming languages have a reputation for being complex. This is probably deserved, but it’s off-putting to novice programmers. One of the most intimidating features about Haskell is monads. In this video we’re going to delve into them in more detail to try and understand what monads are really all about. Firstly, cue some relaxing music.

0:38
Now let’s start off with the following high level definition. Monads allow sequencing of function calls to be enforced by the type system. Is that enough? Well, perhaps it’s enough to be getting on with for now. Monads allow computations to be chained together. Effectively, a monad is a computation pattern as Katie from Facebook told us last week. To reassure you further, we’ve already encountered at least two examples of monads on the course. The IO monad. In week two of the course we studied IO actions like putStrLn and getLine. We looked at the do construct for sequencing these actions, so the result of one IO action could become the input of a subsequent IO action. For example, look at this code.

1:39
This is a use of the IO monad wrapped up as a do block. Secondly, the maybe monad. In week three of the course we studied maybe values, which could be Just x or Nothing. These are used to represent values from computations that may fail. Again, we can use do blocks to order a sequence of computations on maybe values. This is the maybe monad. Look at this block. What value does it return?

2:24
Note that the return keyword here put the final computed value in an appropriate context. More details in the next step. Here’s another do block.

2:39
What value does it return? The Nothing value propagates through the do block and is returned. We are going to investigate the properties of monads over the next few steps on the course to try and understand them in some more detail. However, I hope it’s moderately reassuring to realize that we’ve already encountered monads in Haskell and understood them at some level.
```

**Monads, as a concept, cause trouble to some novice Haskell programmers. How and why do we need to understand this abstract mathematical formalism?**

Over the next few steps, we are going to explore monads in some detail. For now, we want to reassure you that we have already used monads — for input/output and Maybe values.

### Introduction to monad theory

#### What is a Monad?

- A monad is an algebraic structure in category theory, and in Haskell it is used to describe computations as sequences of steps, and to handle side effects such as state and IO.
- Monads are abstract, and they have many useful concrete instances.
- Monads provide a way to structure a program.
- They can be used (along with abstract data types) to allow actions (e.g. mutable variables) to be implemented safely.

##### Building blocks of a monad

A monad has three building blocks:

- A type constructor that produces the type of a computation, given the type of that computation’s result.
- A function that takes a value, and returns a computation that—when executed—will produce that value.
- A function that takes two computations and performs them one after the other, making the result of the first computation available to the second.

Let’s restate this more precisely:

##### Definition of a monad

A monad consists of three objects, which must satisfy the _monad laws_. Let’s look at the objects first:

- A type constructor , such that for any type , the type is the type of a computation in the monad that produces a result of type .
- A function . Thus if , then is a computation in that, when executed, will produce a value of type .
- A function .
  - The first argument is a computation that produces a value of type .
  - The second argument is a function that requires an argument of type and returns a computation that produces a value of type .
  - The result is a computation that will produce a value of type . It works by running the first computation and passing its result to the function that returns the second computation, which is then executed.

#### Monads and Type classes

- Monads are abstract and general and useful.
- Consequently, there are many instances of them.
- This is captured by defining a type class for monads, along with many standard instances. And you can define your own.

##### The _Monad_ type class

```hs
    class Monad m where
        return :: a -> m a
        (>>=) :: m a -> (a -> m b) -> m b
        (>>)   :: m a -> m b -> m b
        fail   :: String -> m a
```

- The `return` function takes a value and returns a monadic value, i.e. a value wrapped in the monad type constructor.
- The `>>=` operator (pronounced “bind”) is a crucial part of a monad. It binds a value returned from a computation to another computation.
- Sometimes the value returned by a computation is unimportant.
- The `>>` operator (pronounced “then”) is like `>>=`, but it just ignores the value returned by the computation:

```hs
    m >> n = m >>= \_ -> n
```

- The `fail` function is used by the system to help produce error messages when there is a pattern that fails to match; normally you don’t use it directly.
- We’ll just pretend `fail` isn’t there.

#### The monad laws

Every monad must satisfy the following three laws. So if something looks like a monad but does not satisfy these laws, it is not a monad! The laws express properties that need to be satisfied in order to make the monadic computations composable.

- The _right unit law_:

```hs
    m >>= return = m
```

- The _left unit law_:

```hs
    return x >>= f  = f x
```

- The _associativity law_:

```hs
    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

##### A monad is what the definition says it is!

- There are many metaphors or intuitions about what a monad is.
- Example: a “computation” or an “action”.
- But these terms are vague—they may help you to understand, but they can also be misleading at times.
- _A monad is exactly what the definition says, no more and no less._

#### The _do_ notation

Writing monadic computations using the bind and then operators is still somewhat clunky. Haskell provides a very convenient syntactic sugar for monadic computations called the “do notation”:

```hs
    baz :: [a] -> Maybe a

    baz xs =
      do  a <- myTail xs
          b <- myTail a
          c <- myHead b
          return c
```

##### Syntax rules for do

```hs
    do { x }  -- >  x

    do {x ; <xs> }  -- >  x >> do { <xs> }

    do { a <- x ; <xs> }  -- >  x >>= \a -> do { <xs> }

    do { let <declarations> ; xs }
      -- >
    let <declarations> in do { xs }
```

### Example: the Maybe monad

- We’ve already seen the Maybe type. Let’s look at the Maybe monad, which makes using the Maybe type a lot easier.

#### The _Maybe_ type constructor

You already know the definition of the _Maybe_ type:

```hs
    data Maybe a = Just a | Nothing
```

#### Example use of Maybe: Safe _head_ and _tail_

The _head_ and _tail_ functions from the Prelude are not safe in the sense that they fail when called on an empty list. We can define safe versions using _Maybe_:

```hs
    myHead :: [a] -> Maybe a
    myHead [] = Nothing
    myHead (x:xs) = Just x

    myTail :: [a] -> Maybe [a]
    myTail [] = Nothing
    myTail (x:xs) = Just xs
```

### Monad instance of Maybe

Now we can make _Maybe_ an instance of the _Monad_ type class, simply by providing the appropriate definitions for _return_, _bind_, _then_ and _fail_:

```hs
    import Control.Monad

    instance Monad Maybe where
        return           =   Just
        Nothing  >>= f = Nothing
        (Just x) >>= f = f x
        fail _           =   Nothing
```

There are a few additional functions defined in the _MonadPlus_ type class:

```hs
    instance MonadPlus Maybe where
        mzero               = Nothing
        Nothing `mplus` x = x
        x `mplus` _         = x
```

That’s it, we now have a _Maybe_ monad!

_Note_: for users of `ghc 7.10` and higher, we need to do [a little bit more work](https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10).

### Explicit Maybe versus the Maybe Monad

Let’s see what this monad gives us:

#### A computation using explicit Maybe

```hs
    foo :: [a] -> Maybe a
    foo xs =
      case myTail xs of
        Nothing -> Nothing
        Just a -> case myTail a of
                    Nothing -> Nothing
                    Just b -> myHead b
```

To combine computations that use the _Maybe_ type, we need explicit `case` expressions to pattern match against the type.

#### A computation using the Maybe monad

Let’s write this computation using the _Maybe_ monad, first using the `>>=` operator:

```hs
    bar :: [a] -> Maybe a
    bar xs =
      myTail xs >>=
        (\a -> myTail a >>=
          (\b -> myHead b))
```

Now let’s change the line breaks and indentation a bit to make it look nicer:

```hs
    bar2 :: [a] -> Maybe a
    bar2 xs =
      myTail xs >>= (\a ->
      myTail a >>=  (\b ->
      myHead b))
```

Thanks to the associativity law, we can also remove unnecessary parentheses:

```hs
    bar3 :: [a] -> Maybe a
    bar3 xs =
      myTail xs >>= \a ->
      myTail a >>=  \b ->
      myHead b
```

This is already a lot cleaner, but finally we can use the `do`\-notation:

```hs
    bar3 :: [a] -> Maybe a
    bar3 xs = do
      a <- myTail xs
      b <- myTail a
      myHead b
```

Clearly, the final monadic code is a lot more readable than the original non-monadic code.

#### Example: Reduction of bar \[5,6\]

```hs
        bar [5,6]

    -- > substitute [5,6] for xs in definition of bar

        myTail [5,6] >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- > def. myTail

        Just [6]  >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- >  def.2 of (>>=)

         (\a -> myTail a >>=
          (\b -> myHead b))
            [6]
    -- > beta reduction, substitute [6] for a

         myTail [6] >>= (\b -> myHead b)

    -- > reduce myTail

         Just [] >>=  (\b -> myHead b)

    -- >  def.2 of (>>=)

        (\b -> myHead b) []

    -- > beta reduction, substitute [] for b

       myHead []

    -- > def.1 myHead

      Nothing
```

#### Example: Reduction of bar \[5,6,7\]

```hs
        bar [5,6,7]

    -- > substitute [5,6,7] for xs in definition of bar

        myTail [5,6,7] >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- > def. myTail

        Just [6,7]  >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- >  def.2 of (>>=)

         (\a -> myTail a >>=
          (\b -> myHead b))
            [6,7]
    -- > beta reduction, substitute [6,7] for a

         myTail [6,7] >>= (\b -> myHead b)

    -- > reduce myTail

         Just [7] >>=  (\b -> myHead b)

    -- >  def.2 of (>>=)

        (\b -> myHead b) [7]

    -- > beta reduction, substitute [7] for b

        myHead [7]

    -- > def myHead

        Just 7
```

### Monad metaphors

There are lots of ways to understand how monads work, and many people have come up with interesting analogies and explanations.

Here are some that we have found particularly helpful, or amusing.

- [A Journey to Enlightenment](http://www.lambdacat.com/the-midnight-monad-a-journey-to-enlightenment/)
- [Understanding Monads](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)
- [Monads are like Burritos](http://blog.plover.com/prog/burritos.html), with accompanying [cartoon](http://chrisdone.com/posts/monads-are-burritos) and a [critique](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)
- [You Could Have Invented Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
- [Monads as Space Suits](http://web.archive.org/web/20081206204420/http://www.loria.fr/~kow/monads/index.html)

### Summary

**A monad is a mechanism for combining computations. It is a typeclass providing the `bind` and `return` operations.**

To be an actual monadic type, the implementations of `bind` and `return` must conform to the three monad laws.

The `Maybe` monad illustrates how to create a simple monad and demonstrates its benefits.

## So long and thanks for all the fun(ctions)!

Now the time has come to say goodbye! We encourage you to continue your functional programming journey.

### Functional Programming in Other Languages
```
0:05
JEREMY: This is the final screen cast of the course. Here we are. What’s happening? I’m going to start a Python interpreter instead of the usual GHCi.

0:20
Shock. Horror. Why am I reverting to Python– Python 3 to be precise. Let me explain. What I want to do in this final screencast is to show you the functional constructs that pop up in mainstream languages. I admit Haskell isn’t a mainstream language yet, and it’s probably true that no functional programming language is in the same league as Python, Java, or C++. However, functional constructs and ideas are being incorporated into imperative and object-oriented languages. So let’s have a little look at Python. Here is a lambda expression. Lambda x goes to x plus 1. This is the increment function. And we can apply this to an argument.

1:16
Here, applying it to 2, and it returns the value 3. Notice the parentheses. I think Python would complain without these brackets. Now, here’s a map function in Python. I’m going to map my same increment function– x goes to x plus 1 over a list of elements 1, 2, 3.

1:48
The thing I get back is a map object. I can either enumerate this object or convert it back into a list. When I convert it into a list, I get back the list 2, 3, 4. Each element is one more than the corresponding element in the input list.

2:07
Here’s another map function.

2:15
Hello plus str x– that converts the input values to strings. I’m going to map over– well, let’s just do 1, 2, 3 for now actually. And if I convert this thing back to a list, I can see what’s going on. Hello 1. Hello 2. Hello 3. So the anonymous function, the lambda function, has turned each input element to a string and prepended hello to it.

2:49
Now, I can do the same thing for lists that have elements of different types in them. Remember, Python is a dynamically-typed language. So here I’m going to say hello and concatenate that to the string representation of whatever I’ve got in my list, which is going to be 1, false, and string jeremy.

3:17
Turn this whole map function into a list, and I get back hello 1, hello false, hello jeremy.

3:26
At this point, you’re probably feeling unhappy. Now you know Haskell, dynamically typed lists seem wrong. Let’s do a filter as well. Filter lamba x goes to x mod 2 equals 1.

3:49
Going to apply this over the range of integers from 1 to 10, not including 10. Again, if I convert this into a list, I can see the answer, which is 1, 3, 5, 7, 9. Actually, I could have used a list comprehension here instead– x for x in range 1, 10 if x mod 2 equals 1. And that returns the same value. For good measure, here’s a fold. I need to import the functools module. And now I can say functools dot reduce. This is the foldl function in Python.

4:38
Lambda a, b, a times b– that’s just a simple multiplication– over the range 1 to 5 with the starting value 1 gives me the product of 1 to 4 inclusive– effectively factorial of 4.

5:01
Other mainstream languages now incorporate lambdas as first-class functions. Examples include Java 8 released in 2014, and C++ 11, released in 2011. In the Comment section below, please let us know which languages you are using and whether you think there is scope to write in a functional style in these other languages.

5:24
Another thing to talk about is syntax. Now you know the clean and elegant syntax of Haskell, does Python seem clunky? Let us know in the Comments section.
```

**All mainstream languages (like Python, Java and C++) are incorporating functional constructs. So even if you aren’t going to be using Haskell regularly, you can still develop your code in a more functional way in other languages.**

Which languages do you use? How can you become more functional when you develop code in these languages?

### Will You Use Haskell in the Future?

**When you started the course, you may not have used the Haskell language before. Now you have spent some time exploring functional programming, do you think you will continue to use it in your day-to-day software development?**

Please let us know which features of Haskell you found to be most interesting and relevant. How might you make use of Haskell in future programming projects?

### The End of the Affair
```
0:08
DR.

0:08
JEREMY SINGER: Wow! We’re at the end of the course. Ohh. But know we’re really happy. Now we are functional programmers. DR.

0:17
WIM VANDERBAUWHEDE: Over the past six weeks, we have been on a learning journey, discovering the Haskell language and the key concepts of functional programming. and I hope that your journey doesn’t stop here, that you will carry on with functional programming. DR.

0:30
JEREMY SINGER: If you want to keep up with Haskell, there are plenty of resources online. Check out some of the links below. The best place to start is probably the haskell.org website. We also recommend you sign up for the Haskell mailing list. DR.

0:43
WIM VANDERBAUWHEDE: However, the main learning objective for this course is to enable you to think functionally. Now that you have this functional perspective, it will change the way you develop code. So you might be using list comprehensions in Python, lambdas in C++, or maps and folds in Java. DR.

1:00
JEREMY SINGER: We hope you’ve enjoyed taking the course. We’ve certainly enjoyed the experience. So a final word of valediction. DR.

1:09
WIM VANDERBAUWHEDE: May your functions always type-check. DR.

1:11
JEREMY SINGER: May your stack never overflow. DR.

1:13
WIM VANDERBAUWHEDE: And may your programs always terminate.
```

Thanks for taking the Functional Programming in Haskell course. We hope you will continue your functional programming journey! May your functions always typecheck! May your stack never overflow! May your programs always terminate!

As you now know, there are lots of online resources to help you program in Haskell. The best starting place is [haskell.org](https://haskell.org). Also check whether there are any [Haskell Meetups](http://www.meetup.com/topics/haskell/) near where you live.

Our final tip is about thinking in a functional way. Don’t _binge_ on functional programming, i.e. take this crash course then go back to how you used to develop code. Instead, let the functional mindset affect your programming style. Do you remember how [Simon Peyton Jones](https://www.futurelearn.com/courses/functional-programming-haskell/10/steps/1103595) predicted that your brain would be rewired by Haskell. Has this happened to you?

#### Acknowledgments

Before we go, Wim and I need to say thanks to lots of people who helped us to make the production of this course so enjoyable … so, we are massively grateful to:

- Andy Sim and the [Media Team](https://www.gla.ac.uk/myglasgow/leads/about/staffteams/mediadigitaldev/) for their video recording and editing
- [Vicki Dale](http://www.gla.ac.uk/services/learningteaching/staff/vickidale/) and [Kerr Gardiner](https://uk.linkedin.com/in/kerr-gardiner-944039b) for their course design and admin advice
- [Frank Coton](http://www.universitystory.gla.ac.uk/biography/?id=WH1322&type=P) for agreeing to fund this project as part of the University of Glasgow’s [Blended Online Learning Development](http://www.gla.ac.uk/myglasgow/leads/staff/telt/blended/) initiative
- [John T. O’Donnell](http://www.dcs.gla.ac.uk/~jtod/) who wrote the original version of the lecture notes that we have adapted for this online course
- [Claire Lipscomb](https://uk.linkedin.com/in/claire-lipscomb-6578a382), our partnership manager at FutureLearn
- [Simon Thompson](https://www.cs.kent.ac.uk/people/staff/sjt/), [Simon Peyton Jones](https://www.microsoft.com/en-us/research/people/simonpj/), [Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/) and [Katie Ots](https://twitter.com/katiejots?lang=en-gb) for letting us interview them
- [Christopher Done](http://chrisdone.com/) for his tryHaskell interactive environment, which we customized for this course
- [Amazon Web Services](http://aws.amazon.com) for providing server credits for our interactive exercises
- the [Haskell](https://haskell.org) designers and developers for making such a great language _and_
- all of you, for being such a great learning community.

We are canvassing opinion on a further course — covering more advanced concepts in functional programming with Haskell. What do you think? Would you take a further course?
