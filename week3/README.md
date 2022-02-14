# Week 3: Data Structures and Types

## Functions on Lists

Lists, recursion and higher-order functions

### Welcome to Week 3

```
0:08
JEREMY: Hi, and welcome back to week three of the Functional Programming in Haskell course. This week, we’ll be exploring the list data structure in even more detail. Then, we’ll move on to defining custom data structures. This is an important part of any high-level programming language. Finally, we’ll dig into the history of Haskell. Where did it come from? Why does it look like it does? We’ll be asking one of the main architects of the Haskell language to answer some of these questions for us. Hope you enjoy the week. See you later.
```

**This week we will go a little deeper into functional programming. The concept of recursion is a key design pattern. You will see that recursion is a natural way to process list data. We are going to look at list combinators, like map and fold. While we are only operating on small lists, these are exactly the operations that analysts use to crunch big data on massive clusters.**

After a brief excursion into tree data types, we will uncover some of the history of Haskell towards the end of the week. Jeremy interviews Simon Peyton Jones, who works at Microsoft Research Cambridge. Simon is an honorary Professor at Glasgow, and one of the lead designers of Haskell.

### Recursive Functions on Lists

#### Computing with lists

- There are two approaches to working with lists:
  - Write functions to do what you want, using recursive definitions that traverse the list structure.
  - Write combinations of the standard list processing functions.
- The second approach is preferred, but the standard list processing functions do need to be defined, and those definitions use the first approach (recursive definitions).
- We’ll cover both methods.

#### Recursion on lists

- A list is built from the empty list and the function . In Haskell, the function is actually written as the operator , in other words _:_ is pronounced as _`cons`_.
- Every list must be either
  - or
  - for some (the head of the list) and (the tail)

where is pronounced as

- The recursive definition follows the structure of the data:
  - Base case of the recursion is .
  - Recursion (or induction) case is .

##### Some examples of recursion on lists

###### Recursive definition of _length_

The length of a list can be computed recursively as follows:

```hs
length :: [a] -> Int           -- function type
length [] = 0                  -- base case
length (x:xs) = 1 + length xs  -- recursion case
```

###### Recursive definition of _filter_

- _filter_ is given a _predicate_ (a function that gives a Boolean result) and a list, and returns a list of the elements that satisfy the predicate.

```hs
filter :: (a->Bool) -> [a] -> [a]
```

Filtering is useful for the “generate and test” programming paradigm.

```hs
filter (<5) [3,9,2,12,6,4] -- > [3,2,4]
```

The library definition for `filter` is shown below. This relies on guards, which we will investigate properly [next week](https://www.futurelearn.com/courses/functional-programming-haskell/10/steps/1103605).

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

#### Computations over lists

- Many computations that would be for/while loops in an imperative language are naturally expressed as list computations in a functional language.
- There are some common cases:
  - Perform a computation on each element of a list:
  - Iterate over a list, from left to right:
  - Iterate over a list, from right to left:
- It’s good practice to use these three functions when applicable
- And there are some related functions that we’ll see later

##### Function composition

- We can express a large computation by “chaining together” a sequence of functions that perform smaller computations

1. Start with an argument of type
2. Apply a function to it, getting an intermediate result of type
3. Then apply a function to the intermediate result, getting the final result of type

- The entire computation (first , then ) is written as .
- This is traditional mathematical notation; just remember that in , the functions are used in right to left order.
- Haskell uses `.` as the function composition operator

```hs
(.) :: (b->c) -> (a->b) -> a -> c
(f . g) x = f (g x)
```

#### Performing an operation on every element of a list: _map_

- _map_ applies a function to every element of a list

```hs
map f [x0,x1,x2] -- > [f x0, f x1, f x2]
```

##### Composition of maps

- _map_ is one of the most commonly used tools in your functional toolkit
- A common style is to define a set of simple computations using map, and to compose them.

```hs
map f (map g xs) = map (f . g) xs
```

This theorem is frequently used, in both directions.

##### Recursive definition of _map_

```hs
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

#### Folding a list (reduction)

- An iteration over a list to produce a singleton value is called a _fold_
- There are several variations: folding from the left, folding from the right, several variations having to do with “initialisation”, and some more advanced variations.
- Folds may look tricky at first, but they are extremely powerful, and they are used a lot! And they aren’t actually very complicated.

##### Left fold: _foldl_

- foldl is _fold from the left_
- Think of it as an iteration across a list, going left to right.
- A typical application is
- The is an initial value
- The argument is a list of values which we combine systematically using the supplied function
- A useful intuition: think of the argument as an “accumulator”.
- The function takes the current value of the accumulator and a list element, and gives the new value of the accumulator.

```hs
foldl :: (b->a->b) -> b -> [a] -> b
```

##### Examples of _foldl_ with function notation

##### Examples of _foldl_ with infix notation

In this example, + denotes an arbitrary operator for f; it isn’t supposed to mean specifically addition.

```hs
foldl (+) z []          -- > z
foldl (+) z [x0]        -- > z + x0
foldl (+) z [x0,x1]     -- > (z + x0) + x1
foldl (+) z [x0,x1,x2]  -- > ((z + x0) + x1) + x2
```

##### Recursive definition of _foldl_

```hs
foldl        :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs
```

##### Right fold: _foldr_

- Similar to , but it works from right to left

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
```

##### Examples of _foldr_ with function notation

##### Examples of _foldr_ with operator notation

```hs
foldr (+) z []          -- > z
foldr (+) z [x0]        -- > x0 + z
foldr (+) z [x0,x1]     -- > x0 + (x1 + z)
foldr (+) z [x0,x1,x2]  -- > x0 + (x1 + (x2 + z))
```

##### Recursive definition of _foldr_

```hs
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
```

##### Relationship between _foldr_ and list structure

We have seen that a list `[x0,x1,x2]` can also be written as

```hs
x0 :  x1 : x2 : []
```

Folding (:) over a list using the empty list \[\] as accumulator gives:

```hs
foldr (:)  [] [x0,x1,x2]
  -- >
  x0 :  x1 : x2 : []
```

This is identical to constructing the list using (:) and \[\] ! We can formalise this relationship as follows:

##### Some applications of folds

```hs
sum xs = foldr (+) 0 xs
product xs = foldr (*) 1 xs
```

We can actually “factor out” the that appears at the right of each side of the equation, and write:

```hs
sum      = foldr (+) 0
product  = foldr (*) 1
```

(This is sometimes called “point free” style because you’re programming solely with the functions; the data isn’t mentioned directly.)

### Functional Maps and Folds versus Imperative Loops

```
0:05
WIM: Hi. In this tutorial, I want to explain the relationship between maps and folds in Haskell and loops in an imperative language. For the imperative language, I will use the scripting language Ruby, but you can do this really in any imperative language. So suppose we want to perform a map on a list using function f.

0:39
And suppose the function f is simply something like x times x plus 1.

0:48
Suppose lst is just a list of numbers.

0:57
Then we can print this computation.

1:27
So, now, if you want to do the same thing in Ruby, then we first need to create the function f.

1:47
And then we create the lists, lst is an empty list. And then we populate it with the range.

2:11
Now, we create the result list.

2:16
And we apply the map.

2:36
And then we simply print this.

2:46
Let’s try this.

2:59
So to get a bit similar result in Ruby, we can move this. And now it looks a bit more like the Haskell we saw. Clearly, the for loop is a very straightforward implementation of the map. So, for every element in the list, we push a new element on to the new list lst_. So what about folds. Now, suppose we have operation g is (/).

3:32
And then we can fold the division and using the left fold, foldl g and then an accumulator, which we can set to 1, and on our list.

3:53
So we can print this now and see what we get.

4:03
So, now, let’s do the same thing in Ruby. We have to create the function g. And we can’t do this neat thing like we do in Haskell, so we have to actually say def g(accumulator and element).

4:19
And we have acc/elt. And then we have a loop.

4:30
And we start with qcc = 1.0. And we apply the function to the accumulator

4:45
And we can try it out.

4:53
Indeed, we get the same thing. Now, if we do the same but with a right fold, so, obviously, the Haskell example is fairly simple.

5:14
And g prime is just the same as g.

5:20
And so we can simply print with the new accumulator. And try that.

5:31
And in Ruby, we need to define a new function g. And I can’t use a prime, so I’ll use an underscore. But note that we have to define it like this.

5:47
And we have our for loop.

5:56
Where we reverse the list, so that we can start at the end.

6:06
We can inspect this.

6:16
And indeed, we do get the same result. So, essentially, what we see is that the fold operations are actually loops that change updatable variable using a function that uses the same variable. So g uses acc and returns and a new value-based on acc and elt, and it sends this to acc. And g_ does the same thing. And you have to explicitly specify the order of the arguments, so that when you traverse the list from the right, you get the same result as in Haskell. For the map, it’s much simpler. So you take a list, you apply the function on each element of the list, and this result is pushed onto the new list.

7:18
Note, again, how much neater all this is in Haskell, so each of these folds and maps are just single lines. In Ruby, we have to write explicit functions for each of them. But the main message is that if you are in some doubt of how a fold, left or a right fold, or a map works in Haskell, you can always try and think of this alternative– how it works in an imperative language with for loops. And from there, you can easily reason about what your maps or folds should look like.
```

**This tutorial explains the relationship between the higher-order list operations `map`, `foldl` and `foldr` and loops in an imperative language.**

The source code can be found on [GitHub](https://github.com/wimvanderbauwhede/HaskellMOOC/tree/master/MapsFoldsLoopsTutorial).

Summary
`map` : loop over list element-by-element, append new element to new list

`foldl` : loop over list element-by-element, update accumulator using current accumulator and element

`foldr` : loop over reverse list element-by-element, update accumulator using current accumulator and element

Note:

```hs
map :: (a -> b) -> [a] -> [b]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldr :: (a -> b -> b) -> b -> [a] -> b
```

### Do it Yourself: Lists and Recursion

**In this interactive tutorial, we will reinforce the concept of list processing with recursion. We will look at using the list processing strategies outlined in the previous steps, including _map_ and _filter_.**

#### Computing with lists

There are two approaches to working with lists:

- Write functions to do what you want, using recursive definitions that traverse the list structure.
- Write combinations of the standard list processing functions.
The second approach is preferred, but the standard list processing functions do need to be defined, and those definitions use the first approach (recursive definitions). We’ll cover both methods.

```sh
>> next
```

#### Recursion on lists

As we have already seen, a list is built from the empty list `[]` and the function cons or in operator form `(:)`.

Every list must be either
`[]` or
`(x:xs)` for some `x` (the head of the list) and `xs` (the tail).
where `(x:xs)` is an alternative syntax for `cons x xs`

The recursive definition follows the structure of the data:
Base case of the recursion is `[]`.
Recursion (or induction) case is `(x:xs)`.

However, in order to create such recursive definitions, we must first see how we can create conditional functions: functions that define both the base case and the induction case.

```sh
>> next
```

#### Defining conditional functions

In Haskell, there are several ways to define conditional functions. The easiest way is to write a definition for each case, as is done in the notes, e.g.

```hs
length [] = 0
length x:xs = 1 + length xs
```

Unfortunately, this style does not work in the web-based environment because it only supports _single-line_ expressions. However, there are other ways. The most straightforward is to use an _if-then-else_ expression:

```hs
length lst =
  if lst == []
    then 0
    else let x:xs = lst in 1 + length xs
```

Alternatively, you can use what is known as "guards", e.g.

```hs
length lst
  | lst == [] = 0
  | otherwise = let x:xs = lst in 1 + length xs
```

This is particularly useful if you have many conditions (similar to a "case" statement in other languages)

Finally, you can use multi-line functions using the where-clause and semicolons:

```hs
f = f' where f' 1 = 0; f' x = x + f' (x-1)
```

All of these can be written on a single line so you can use them in the web-based environment. So go ahead and define your own `length` function, and try it out: `length ['a','c' .. 'w']`

```sh
>> length ['a','c' .. 'w']
12
:: Int
```

#### Recursive definition example: `filter`

The function `filter` is given a predicate (a function that gives a Boolean result) and a list, and returns a list of the elements that satisfy the predicate.

Filtering is useful for the “generate and test” programming paradigm.

Try this `filter (<5) [3,9,2,12,6,4] -- > [3,2,4]` or create your own example.

```hs
>> filter (<5) [3,9,2,12,6,4]
[3,2,4]
:: (Num a, Ord a) => [a]
```

Now define your own filter function and use it to filter the example list `[3,9,2,12,6,4]`.

```hs
>> filter (<5) [3,9,2,12,6,4]
[3,2,4]
:: (Num a, Ord a) => [a]
```

Well done, your filter function works correctly!

A possible recursive definition is:

```hs
filter pred lst
  | null lst = []
  | otherwise = if pred x
     then x:filter pred xs
     else filter pred xs
       where x:xs=lst
```

Or on a single line: `filter pred lst | null lst = [] | otherwise = if pred x then x:filter pred xs else filter pred xs where x:xs=lst`

### Do it Yourself: Function Composition

**In this interactive tutorial, we will explore the behaviour of `foldl` and `foldr` in some detail. We will also investigate function composition with the `.` operator. This leads onto _point free_ style.**

#### Computations over lists

Many computatations that would be for/while loops in an imperative language are naturally expressed as list computations in a functional language.

There are some common cases:

- Perform a computation on each element of a list: `map`
- Iterate over a list, from left to right: `foldl`
- Iterate over a list, from right to left: `foldr`

It’s good practice to use functions like `filter` and these three functions when applicable.Let's look at maps and folds in some more detail.

#### Function composition

As explained in the notes, we can express a large compution by “chaining together” a sequence of functions that perform smaller computations using the `(.)` operator, e.g. `f . g`. This operation is particularly useful in the composition of `map` operations. A common style is to define a set of simple computations using map, and to compose them.

The following relationship is very useful to refactor your code:

```hs
map f (map g xs) = map (f . g) xs
```

This theorem is frequently used, in both directions. For example, if we want to take a list of numbers and perform two operations on each number, we could write:

```hs
map (+5) (map (*3) [1..10])
```

But we could equally write:

```hs
map ((+5) . (*3)) [1..10]
```

Now write your own example of the use of `map`

```sh
>> map ((+5) . (*3)) [1..10]
[8,11,14,17,20,23,26,29,32,35]
:: (Enum b, Num b) => [b]
```

#### Folding a list (reduction)

An iteration over a list to produce a singleton value is called a `fold`. There are several variations: folding from the left, folding from the right, several variations having to do with “initialisation”, and some more advanced variations.

Folds may look tricky at first, but they are extremely powerful, and they are used a lot! And they aren’t actually very complicated.

The left fold (`foldl`) processes the list from the left. Think of it as an iteration across a list, going left to right. A typical example is e.g.
`foldl (\acc elt -> elt:acc) "" "Reversing a string"`
which unsurprisingly reverts a string.

Now go ahead and define your own example using `foldl`.

```sh
>> foldl (\acc elt -> elt:acc) "" "Reversing a string"
"gnirts a gnisreveR"
:: [Char]
```

The right fold (`foldr`) is similar to foldl, but it works from right to left. Some typical examples are:

`sum xs = foldr (+) 0 xs` and
`product xs = foldr (*) 1 xs`
What happens if you replace `foldl` with `foldr` in the string reversal example?

`foldl (\acc elt -> elt:acc) "" "Reversing a string"`

```sh
>> foldr (\acc elt -> elt:acc) "" "Reversing a string"
Couldn't match expected type ‘Char’ with actual type ‘[Char]’
In the first argument of ‘(:)’, namely ‘elt’
In the expression: elt : acc Couldn't match type ‘Char’ with ‘[Char]’
Expected type: [[Char]]
Actual type: [Char]
In the third argument of ‘foldr’, namely ‘"Reversing a string"’
In the expression:
foldr (\ acc elt -> elt : acc) "" "Reversing a string"
In the expression:
let in foldr (\ acc elt -> elt : acc) "" "Reversing a string"
```

The result is an error, because `foldr` and `foldl` expect different types of functions:

- `foldl` expects a function that takes as first argument the accumulator and as second argument the element of the list. The type signature is

```hs
foldl ::  (a -> b -> a) -> a -> [b] -> a
```

- `foldr` expects them in the opposite order, its type signature is

```hs
foldr ::  (a -> b -> b) -> b -> [a] -> b
```

So go ahead, change the order and try again. What do you get?

`foldl (\acc elt -> elt:acc) "" "Reversing a string"`

```sh
>> foldr (\acc elt -> acc:elt) "" "Reversing a string"
"Reversing a string"
:: [Char]
```

As you can see, you get the same string back, no reversal occured. Can you change the definition to get a reversed string with foldr?

`foldr (\elt acc -> elt:acc) "" "Reversing a string"`

```sh
>>
```

#### Point-free style

We can actually “factor out” the `xs` that appears at the right of each side of the equation, and write:

`sum = foldr (+) 0`

`product = foldr (*) 1`

This is called "point free" style,it means that you're programming solely with the functions; the data isn't mentioned directly.

Try this out for yourself and see if you can define other functions using this style.

### What Have We Learned About Lists?

How much have you understood computations with lists, as covered in the article and tutorials? This quiz will test your knowledge.

#### Question 1

A _recursive_ function must have at least two cases. What are these called?

- [ ] base case and general case
- [ ] general case and specific case
- [x] base case and induction case

#### Question 2

What is wrong with the following definition of `filter`?

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter pred []    = []
filter pred (x:xs)
    | pred x         = x : filter pred xs
    | otherwise      = filter pred (x:xs)
```

- [ ] The base case is wrong
- [ ] The predicate should operate on `xs`, not on `x`
- [x] The recursion for the non-matching case should operate on `xs`, not on `(x:xs)`
- [ ] The recursion for the matching case should work on (x:xs), not on xs

#### Question 3

What is the effect of the following fold?

```hs
foldl (\acc elt -> acc++[elt]) "" "A string"
```

- [ ] It will return “gnirts A”
- [x] It will return “A string”
- [ ] It will return the string except its last character

#### Question 4

What is the wrong with the following map/fold-based computation?

```hs
foldl (+) (map (*2) [1..8])
```

- [ ] The `map` and `foldl` functions should be swapped
- [x] foldl needs an accumulator argument.
- [ ] map should take a function like (*), not (*2)

#### Question 5

What is the result of the following computation?

```hs
foldr (/) 1 [2,4,8]
```

- [ ] 0.25
- [ ] 0.5
- [ ] 2.0
- [x] 4.0

#### Question 6

What is the result of the following computation?

```hs
foldl (/) 16 [8,4,2,1]
```

- [x] 0.25
- [ ] 0.5
- [ ] 2.0
- [ ] 4.0

### Writing a Spelling Book Generator

**Are you familiar with children’s alphabetical spelling books? They say things like: _a is for apple, b is for baby, and c is for cat_**

Now that you know about Haskell list functions, you can develop a function to generate the text for a spelling book, given a list of words.

Suppose the function is called `speller` then it should have the following type:

```hs
speller :: [[Char]] -> [Char]
```

recalling that a list of `Char` elements is a `String` in Haskell.

Typical executions of the `speller` function would look like this:

```hs
speller ["abacus"] -- > "a is for abacus"
speller [] -- > ""
speller ["apple", "banana", "coconut"]
 -- > "a is for apple, b is for banana, and c is for coconut"
speller ["whisky", "x-ray"]
 -- > "w is for whisky, and x is for x-ray"
```

Your task is to develop the Haskell `speller` function. You are allowed to define small _helper_ functions, perhaps to generate a single letter’s phrase from a word: `f "zoo" -- > "z is for zoo"`. You are also allowed to use the standard Haskell list functions like `map` and `foldr` if appropriate.

Write your Haskell source code in a .hs file, and copy/paste it into the exercise activity below. At the end of this week, we will ask learners to peer-review submitted code anonymously, by comparing it with other submissions.

When you write your code, please pay attention to:

- type declarations for functions
- pattern matching for different list structures (i.e. empty list, single element list, multi-element list)
- neat formatting of code
- concise, explanatory comments

### Summary

**The basic mechanism for computing on any datastructure in Haskell is recursion. Recursion always has a _base case_ and an _induction case_. For lists, the base case is the empty list `[]`, the induction case is adding an element to the list `x:xs`**

For list operations, it is usually easier to use higher-order functions like `map` (performing an operation on every element of a list) and `foldl/foldr` (reducing a list to a single value). Sometimes these functions are referred to as _list combinators_.

## Custom Data Types

Defining your own datatypes, in particular trees

### Define Your Own Data Types

```
0:05
JEREMY: Let’s talk about data types. A type is a set of values that are related, a family of values that belong together. So far we’ve seen the Bool data type, which consists of values True and False. We’ve also used the Int data type, which consists of whole number values from some minimum to some maximum. These bounds may differ based on whether your OS is 32-bit or 64-bit. What else have we looked at? Characters, lists. Oh, yes, and pairs as well. We’ve also looked at function types that describe argument and return values. Now, we want to think about user-defined types, creating custom data types for our own program.

0:57
First, we’ll consider a simple type that consists of a finite set of alternative values. Apparently, in the Amazon jungle, there is a tribe who count one, two, many. That is, they have no distinct words for larger integers than two. We could represent this type in Haskell as follows– data SimpleNum equals One or Two or Many. If you have GHCi open, why not type this in your interactive session right away? The key word “data” indicates we are defining a new type. The name of the type and the names of the values should start with capital letters. The alternative values are separated with a vertical bar character. Let’s have a look at some of these values. One– ah, we can’t see it.

2:01
We need to be able to print it out. Add deriving Show to the type definition.

2:10
Show is a type class. We’ll talk in more detail about this later. For now, let’s just understand that any type must derive Show if we are to print out its values.

2:26
Data SimpleNum equals One or Two or Many deriving Show. OK.

2:40
Great. Now, let’s write a function to convert from int to SimpleNum values. First, I’m going to turn on multi-line support in GHCi so I can format my function definition over several lines. I use the set command to do this. OK. Let’s go for it. Let convert 1 equal One, convert 2 equal Two, convert underscore equal Many. So convert takes an int input– well, really a value that has a type belonging to the Num data class– again, more later– and return a SimpleNum output. Let’s try this out.

3:32
Convert 1.

3:36
Convert 300. OK. Map convert 1 to 5. Perfect.

3:51
Right. That’s a custom data type with alternative values, otherwise known as a Sum data type. Now let’s think about a record data type that stores a portfolio of values. Hm. How about cricket scores? When a team bats in cricket, you need to know two integer values. The first is the number of runs scored by the team. The second is the number of players who are out, i.e. have been eliminated. So a good score for the New Zealand cricket team might be 350 for 4. That’s 350 runs scored for the loss of four players. We can represent this as a Product data type. Data CricketScore equals Score Char list int, int, deriving Show.

4:58
Score is a type constructor that takes a string and two int arguments and returns a cricket score value.

5:12
In general, these kinds of custom data types are called algebraic data types. The alternative values relate to algebraic sums, and the record values relate to algebraic products. I’ll spare you the hairy Type Theory for now. But at least you know to Google for algebraic data types if you are keen to discover more about type theory. Let’s conclude with what we’ve learned in this session.

5:45
First, we use the “data” keyword to define a new custom type. Second, types must derive the Show type class if we want to see their values printed out. Third, we used vertical bars to specify alternatives, or sum data types. And we used type constructors to build record types like score values in cricket, which are product data types. Thanks. Goodbye.
```

**So far we have looked at the basic built-in data types like Int and Boolean. We can combine these basic types to generate custom data types, using algebraic sums and products.**

### Grow a Tree

**In Computer Science, trees grow upside-down. The unique _root_ is at the top, and the _leaves_ are at the bottom.**

The binary tree data type is often used for storing data in a sorted order, to allow efficient searching — for example, a telephone directory.

We are going to define a Haskell data type for trees, storing integer values.

```hs
data Tree = Leaf | Node Int Tree Tree deriving Show
```

A `Tree` value might be either a `Leaf` or a `Node`. Note that this is a _recursive_ data type, since a `Node` stores an `Int` payload and has branches to two subtrees (sometimes called children).

Here is the simplest tree — it’s just a single leaf.

```hs
Leaf
```

Here is a tree with one `Node` containing value 3, and two leaves.

```hs
Node 3 Leaf Leaf
```

If you type this into ghci, you will see the values returned when you construct these trees, so long as your `Tree` datatype derives the `Show` type class.

Look at the type of this value in ghci :

```hs
let l = Node 3 Leaf Leaf
:t l
```

Look at the type of the constructor Node:

```hs
:t Node
```

This is a function: the constructor `Node` takes three arguments and returns a `Tree` result.

Now let’s write a function to compute the depth of a `Tree` — this is the maximum number of branches from the root to any leaf. To write this function, we will do pattern matching on the different kinds of `Tree`, i.e. `Leaf` and `Node` values. Each `Leaf` is a base case, but for each `Node`, we need to recursively process the two child trees.

```hs
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)
```

Notice the `_` in line 3, which is a ‘don’t care’ value, since we discard the `Int` payload in each `Node`.

You can imagine how to write a very similar function that traverses a tree and adds up all the values in its nodes. Have a go at writing this, maybe call it `treeSum`.

Here is its type:

```hs
treeSum :: Tree -> Int
```

How about a function to check whether a tree is sorted properly? The data structure invariant we want is that, for any `Node` storing value `x`, all values in its left subtree are `< x`, and all values in its right subtree are `>= x`.

So this function will take in a `Tree`, a minimum value, a maximum value and it will return a `Bool`. isSortedTree :: Tree -> Int -> Int -> Bool

A `Leaf` is automatically sorted, since it does not contain a value. For each `Node`, we have to check the value in it is between the min and max values, which start off as far apart as possible, then get split into smaller ranges based on the value at the `Node`.

```hs
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted   = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted
```

You can download this Haskell source code below, in file sortedtree.hs. Load the file into ghci: :l sortedtree.hs

(assuming you are in right directory) then invoke the function from the ghci prompt:

```hs
isSortedTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) minBound maxBound
```

where `minBound` is defined as the smallest possible `Int` value, and `maxBound` is the largest.

Let’s look at one more function for now. So far, we have studied tree _traversal_ functions, where we go through the tree data structure and do some incremental computation at each node. Now we want to make a tree _modification_ function. This generates a new `Tree` that is a modified version of the input `Tree`.

The particular function we are going to define inserts a new _maximum_ value. We go through the input `Tree` until we find the rightmost node with a `Leaf` on the right, then we then replace this rightmost `Leaf` with a new `Node` containing a new max value (one larger than previous max value).

```hs
addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree
```

This `addNewMax` function takes a `Tree` input value and returns a `Tree` output value. You can download this code from `addnewmax.hs` below, and load it into ghci. Note the function does not do a _destructive_ update — the old input `Tree` remains unchanged. We have created a new data structure, which has some nodes shared with the original `Tree`. Search online for _purely functional data structures_ to find out more information about this concept.

Next you need to write some `Tree` functions for yourself. Download the `sortedtree.hs` file below and load it into GHCi. Can you write functions to insert a value into a `Tree` in order, or to convert from a `Tree` into a list?

### Type Classes

```
0:05
JEREMY: Hi. In any high-level programming language, there are various number like types. In Haskell, for instance, we have seen Int so far. There are also float and infinite precision integers, like BigIntegers in Java to name a few more types. Now, here is a key insight. Many arithmetic functions can be applied to values from any number like type, for example, addition or multiplication. Here’s 2 + 2, which makes 4. Here is 2 + pi, which makes 5.1415, and so on. Here’s 2 * 2, which makes 4. And here’s 2 * pi, which makes 6.2831, so on. This is where the notion of type classes comes in handy. A type class specifies a family of types that provide implementations for common functions.

1:22
Let’s have a look at the type of (+).

1:32
Sorry, I need to put (+) inside brackets because it’s an infix operator.

1:40
This says, given a type a– some people pronounce this as alpha– given a type a belonging to the Num type class, then the + function takes two a parameters and returns an a result. a is a type variable. The part of the type before the double arrow here is called the context of the type. This expresses type class membership for type variables like a. So a type class is a generalized family of similar types.

2:33
Let’s think about some other the type classes. The Eq type class means we can compare values of such types equality with the (==) operator.

2:50
We see that given the context of the type a that is an instance of the Eq type class, then the (==) function takes two parameters of type a and return to Boolean result. So we can see that ints are an instance of the Eq type class.

3:19
The Ord type class means we can order values of such types with relational operators like less than and greater than.

3:33
This is like the IComparable interface in C#.

3:45
For strings, less than implements a lexicographic ordering.

3:54
The Show type class means we can generate string values that represent values of such types, like toString() in Java. So if I say, show 42 integer value, then I get back the string 42.

4:16
If I say show False, I get back the string False.

4:28
The Read type class means we can generate values of such types from string values. So I’m going to say read “1” so string. Now, this doesn’t work at the moment because I need to say that the thing I’m reading should be of type Int. Now, it works. Again, I could say read “True,” which should be of type Bool, and I get back a Boolean type– rather, a value of a Boolean type.

5:18
To specify that a type belongs to a type class, for now, we will use the deriving clause in the type definition. Let’s go back to our SimpleNum class. Remember, we can only count values One, Two, and Many. Data SimpleNum = One, Two, Many. And this type is deriving (Show, Read).

5:59
The set of type classes that this type is an instance of are in parentheses in the deriving clause. Now, this means we can convert SimpleNum values into strings using the show function, and vice versa using the read function. Let’s say show One, show Two, show Many. And in each case, we return a string.

6:33
Let’s try to read “One”. We get an error because read doesn’t know what the type of One to be– the string One to be– when we read it. So let’s say we want this to be of type SimpleNum, and how it works.

6:53
Let’s try comparing SimpleNum values for equality.

7:02
Does One == One? No. Look at the error message. SimpleNum types are not instances of the Eq type class. So let’s add the Eq type class to the deriving clause in the SimpleNum type definition. Look, now, we have three type classes (Show, Read, Eq).

7:34
Does One == One now? Yes, it does. Does One == Two? No, it doesn’t. Does One == “One”? No, because SimpleNums are of a different type two strings, which a character lists.

8:03
Again, there’s a default implementation of ==, which is being used to compare SimpleNums.

8:11
There is a mechanism for defining type class function implementations, but we’ll save this discussion for later on in the course.

8:22
Note that type classes were one of the early innovations of the Haskell programming language. The type class constrains member types to provide functions that conform to certain type signatures effectively, API constraints. This is a little like object-oriented programming. Type classes are like interfaces in C# and Java. Types in the type class are like concrete implementations of the interface. Type classes provide a neat mechanism to enable operator overloading in the Haskell language. We’ve seen this with functions like ==, and relational operators, and the show and read functions. We’ll explore type classes in more detail later on in the course. But for now, goodbye.
```

**Inevitably, we have glossed over some of the more complex parts of Haskell while we have been introducing the language. Now it’s time to explore one of the ideas we have bumped into on several occasions already — type classes.**

What’s the type of 42? Either type 42 into the [online interpreter](https://www.haskellmooc.co.uk/) or enter

```sh
:t 42
```

in GHCi. Either way, you should get back `Num a => a` — whereas you might have thought the answer should be `Int`. The `Num` is a type class, which specifies a family of types including integer and floating-point types.

## Haskell History

Diving into the history of the Haskell language, we explore how and why it was invented.

### Interview with Simon Peyton Jones

```
0:07
JEREMY: Hey, so Simon, welcome back to Glasgow. How many years did you spend working here?

0:13
SIMON PEYTON JONES: It was about nine, but it felt much longer at the time. This is also the time my children were being born and it was a very important time of change for the department. So it was a very exciting time.

0:22
JEREMY: Well, as well as your children being born, the Haskell movement, I suppose, was born.

0:26
SIMON PEYTON JONES: It was. It slightly predated my children, actually. So they refer to Haskell as my first child.

0:31
JEREMY: Oh I see. Very good. Now I think in the past you’ve said Glasgow is a kind of spiritual home for Haskell. Are those your precise words?

0:42
SIMON PEYTON JONES: I don’t know, but I kind of feel that. Because Haskell was the fruit of an international committee of about 20 people. But it just so happened that several members of the committee were here at Glasgow. So it was John Hughes, Phil Wadler, myself, Kevin Hammond, who became an editor of the report. And Will Partain became involved a little later. So there was quite a few members of the Haskell Committee who were just here at Glasgow. So it was a sort of epicenter, if you like.

1:09Skip to 1 minute and 9 secondsJEREMY: The epicenter, that’s a nice word. Very good, yes. Now you spoke about the committee. And I think in terms of programming languages, being designed by committee is not always a good thing.

1:22Skip to 1 minute and 22 secondsSIMON PEYTON JONES: It’s not usually regarded as being an advantage. Because all of the languages that you hear about, Ruby, and Perl, and Python, and so forth, they all have quite clearly distinct individual closure. They have individual people who designed them. Or Pascal even longer ago. So Haskell’s very unusual in being, A, designed by a group, but B, nevertheless being quite successful over the long term.

1:46Skip to 1 minute and 46 secondsJEREMY: There’s a wonderful quote from Tony Hoare– you were reminding me of it before– where he said he thought Haskell was doomed to succeed. Would you care to measure the success of Haskell? Do you think it’s exceeded your wildest dream?

2:02
SIMON PEYTON JONES: Oh, it certainly has. So I think what was in Tony’s mind is that Tony really likes very small, conceptually pure languages that are sort of stripped down to their essence. And Haskell, right from the beginning, it has a conceptual core that’s very small, but it has quite a lot of stuff around the outside. There’s more than one syntax for doing this. There’s lets as well as wheres. There’s pattern matching as well as case expressions. And we thought that was important. So I think he felt it was doomed to succeed because it was made by a larger group. But in fact no language is doomed to succeed.

2:32
I think my expectation was that Haskell would become a useful research vehicle for a number of research groups who were working on lazy functional programming at the time and maybe no more than that. But in fact, it’s been solidly influential over 2 and 1/2 decades. And it’s become a kind of thought leader in a way that has definitely exceeded my expectations.

2:53
JEREMY: You mentioned laziness there. And I think that’s one of the unusual things about Haskell compared to most mainstream programming languages. Can you give us a kind of Idiot’s Guide to Laziness, please?

3:05
SIMON PEYTON JONES: So laziness was, if you like, the thing that brought that particular group of people together, was the single war cry that sort of identified what Haskell was at the time. So lazy evaluation means, let’s procrastinate as much as possible. Let’s not do any work unless we absolutely have to. So if you call a function and you’re passing it an argument, well, let’s not evaluate the argument yet. Let’s pass to the function, the kind of recipe or suspension, which when the function needs the argument, it can evaluate it. And if you want to regard if, then, else as a function, that’s rather important. Because you don’t want to do both the then part and the else part.

3:37
Because you only want to do one in them. They’re kind of crucially important. But then it turned out to be more than just convenient. John Hughes wrote this very influential paper– John Hughes, also here at Glasgow– called Why Functional Programming Matters, in which he described how lazy evaluation allows you to compose programs together in a way that you couldn’t. You can write a program that generates all the possible moves in a game tree, sort of all the possible alternatives. That is an infinite tree. And then separately, you could write a program that walks over the tree and picks the right one. And in a eager language, you have to meld those two passes together into one.

4:10
But in a lazy language, you can separate them. So it’s a powerful modularity mechanism. And also it’s a powerful purity mechanism. It sort of forces you to be pure, which retrospectively I think was the most important thing about it.

4:21
JEREMY: Good. There are noises in the Haskell community about becoming more strict these days. Is that something you approve off?

4:30
SIMON PEYTON JONES: Oh yes, the whole strict-lazy dichotomy. So initially it was, well, there was Haskell, which was a lazy language, and there was Miranda, and SASL, and a few others, and there were strict languages. And these were very sort of different camps. But over time, Haskell has grown more strict, we have strictness annotations to allow the programmer more control over evaluation order. And indeed, the strict languages also usually have some kind of thunking mechanism to allow you to model lazy evaluation. So in fact I think people now see it more as a continuous spectrum. It’s more, what’s the default, rather than a sort of way of life doctrine.

5:08
JEREMY: Yes, I’d like to ask you about Haskell applications out in the real world. Can you think of any examples of perhaps very exotic deployments of Haskell programs?

5:21
SIMON PEYTON JONES: Oh well, so Haskell is kind of strange, because it’s been quite a niche language. So it’s been difficult for large companies to start using Haskell in a major way, because they’re always worried about how many Haskell programmers there’ll be and will the Haskell compiler continue to be supported? So I think that Haskell as kind of like a fungus or a mushroom. You know the way that fungi have these very long tendrils under the ground. So they’re quite widespread, but you can’t see them?

5:49
JEREMY: Yeah. So Haskell is quite widely used, particularly in small startup companies with the chief technology officer– Like there’s a company called SchedMe in New York doing some kind of scheduling system. And they’re entirely built in Haskell. And their CEO is telling me how they threw away their Ruby code, and it was just so much better in Haskell. So it’s used in small companies quite a bit. It’s also used in some big companies, particularly banks. Every bank appears to have a secret functional programming group. And quite often, that functional programming group is using Haskell. So Standard Chartered, for example, has quite a big Haskell group. In fact, they’re continually hiring people.

6:27
And they don’t get their quants, their traders, to write Haskell directly, but a lot of their infrastructure is written in Haskell. And when their quants say, we want some new kind of spreadsheet-oriented tool, they’ll write spreadsheets that invoke functions written in Mu, which is essentially a reimplementation of Haskell. So Haskell has become quite important in a number of financial settings. And then Galois, founded by John Launchbury, who is also a graduate of this department. So is now quite a successful company in the northwest United States. And I was talking to Alice Miller earlier on today. They do a lot of safety critical things using Haskell.

7:07
One example was they have a domain-specific language for writing the control systems for cool quad copters. So they write in this very high-level language, which is essentially Haskell, and then they compile that down to C, which runs on the quad copter.

7:20
JEREMY: Here’s a slightly cheeky question, Simon. What’s your second favorite programming language after Haskell presumably is your first?

7:26
SIMON PEYTON JONES: Difficult question, yes, because they’re a long way behind. I think I’d have to say Excel.

7:34
JEREMY: Right? That’s interesting.

7:35
SIMON PEYTON JONES: Most people don’t think of Excel as a programming language at all. But it is, right? And moreover, it’s a functional programming language, which everybody believes should be fun– They don’t even think of it as func– They don’t even think they’re programming. They’re modeling. So what I would like to do is to get Excel to be more up front about being a functional programming language, and particularly to support user-defined functions and rich data structures, records and arrays ways and so forth. And then in my grand vision, we would teach Excel as our first programming language. Because anything you can do in Haskell, ultimately you could do in Excel if you could do all of this.

8:07
That would be pretty exciting. So my second favorite programming language is not Excel as it is today, but Excel as I hope it might be one day.

8:15
JEREMY: Good. This is my last question now, Simon. You’ve written an article about how Haskell is like wearing a hair shirt. Now I want you to give some encouraging words to novice Haskell programmers, people who are learning about Haskell in our course and maybe finding it quite difficult. What would you say to encourage them to pursue functional programming?

8:41
SIMON PEYTON JONES: Functional programming, particularly if you’re used to writing in imperative programs, does feel a bit like rewiring your brain, which can feel a little bit painful at first. But the experience is once you come through that pain, you look at the entire– functional programming is kind of like a radical and eloquent attack on the entire enterprise of writing programs. It’s not like, well, I’m just going to switch syntax. It’s just C# instead of Java. It’s a whole different way of thinking about the problem. And if you think about complex problems, and programs are the most complex artifacts that human beings have ever built.

9:11
If you think about complex problems in a new way, that could shed really new light on it. So I think one of the interesting things about Haskell in particular, is it sticks remorselessly purely to one central idea, which is a programming with values and functions rather than programming with mutation and side effects. And most other functional programming languages sort of dice with the devil a bit and do quite a lot of side effects. Haskell is pretty remorselessly pure about that. And that kind of educates you continually into this new way of thinking. I’ll just give one tiny example. Sometimes you do want to do mutating things, and we do that using monads.

9:49
When you want concurrent threads that are doing I/O in the world, so they must be concurrent, they must interact with each other in some stateful way, a good way to make them interact is using transactional memory. So transactional memory is something that’s quite hard to do in imperative languages, because in principle, every mutation of every memory location must be tracked. But in Haskell, because the mutation of mutable locations are so carefully partitioned in the type system– and a little bit awkward to get at as a programmer– then there are very few of them. So transactional memory systems are efficient and effective. And indeed they come out of the box with GHC.

10:26Skip to 10 minutes and 26 secondsJEREMY: Simon, thank you very much.

10:28Skip to 10 minutes and 28 secondsSIMON PEYTON JONES: It’s been great talking to you. It’s really nice to be back in Glasgow actually.
```

**Jeremy interviews Simon Peyton Jones, lead designer of the Glasgow Haskell Compiler**

### Brief History of Haskell

Once upon a time, there was a mathematician named Alonzo Church at Princeton University. Church was Alan Turing’s PhD supervisor. Church devised a mathematical model of functions called lambda calculus. Yes, this is where modern-day lambdas come from!

We will explore the details of lambda calculus later in the course. For now, all we need to know is that lambda calculus captures the _essence_ of computation. It involves function abstraction (like defining functions in Haskell) and application (like calling functions in Haskell).

Fast forward from Church in the 1930s to the early development of programming languages in the 1950s. One of the first high-level programming languages was LISP (which stands for List Processing). LISP adopted a functional style. It allowed user functions to be defined, and passed around as values. LISP lives on … more recent incarnations include Scheme and Clojure.

Fast forward again. During the 1980s, lots of researchers were inventing and extending various functional programming languages. Example languages include ML, Hope and Miranda. However research was fragmented across the various languages, and many of them did not have ‘open-source’ frameworks. So a group of academics formed a committee to design and implement a new language, which would be used as a vehicle for research as well as for teaching functional programming.

After several years of work and arguments, the committee published the first [Haskell Language Report](https://wiki.haskell.org/Language_and_library_specification) in 1990. This was a major milestone: at last there was a common functional language around which the research community could unite.

The language has grown in popularity ever since, despite an avowed aim to _avoid success at all costs_. There are several freely available implementations. The most commonly used is the Glasgow Haskell Compiler, which has an interpreter (ghci) and a compiler (ghc). These form an integral part of the [Haskell Platform](https://www.haskell.org/platform). Lots of people contributed to this software ecosystem. Many of them have worked at the University of Glasgow like [Simon Marlow](https://en.wikipedia.org/wiki/Simon_Marlow), [Simon Peyton Jones](https://en.wikipedia.org/wiki/Simon_Peyton_Jones), and [Phil Wadler](https://en.wikipedia.org/wiki/Philip_Wadler).

Haskell is now widely used in teaching, research and industry. For instance, it’s taught at several Scottish universities including Glasgow, Edinburgh and St Andrews. It has its own annual research conference, the [ACM Haskell Symposium](https://www.haskell.org/haskell-symposium/). And there are many industrial users, including at Facebook. We will be interviewing a software developer from Facebook within the next few weeks.

Optionally, if you want to learn more about the history of Haskell, please check these links:

- read the [History of Haskell paper](https://wiki.haskell.org/History_of_Haskell)
- watch this [lecture by Simon Peyton Jones](https://www.youtube.com/watch?v=3bjXGrycMhQ)

### End of Week 3

```
0:09
JEREMY: Algorithms plus data structures equals programs. This famous equation was introduced by Niklaus Wirth in the 1970s. In terms of Haskell programming, you now know enough about algorithms and data structures in order to create meaningful Haskell programs. As far as algorithms go, you have looked at recursion and also at mapping simple functions over lists of data elements. And in terms of data structures, we’ve spent lots of time exploring lists. And this week we also introduced the tree data structure as well as giving you the apparatus to define your own custom algebraic data types. Next week we are going to think about cannibal programs that eat other programs as their input data. These are called parsers. Well done.

1:19
We’re now halfway through the functional programming course. You’re doing really well. We’ve got three weeks left. Please leave your comments in the section below, and I look forward to seeing you again next week. Bye
```

**At the half-way stage of the course, you now have a grasp of the main concepts in Haskell. We have applied these concepts in building simple programs, as well as exploring the historical motivation for functional programming.**
