# Week 5: Hardcore Haskell

## Laziness and Infinite Data structures

We explore what lazy evaluation is and how you can use it to create infinite data structures

### Welcome to Week 5

```
0:08
WIM VANDERBAUWHEDE: Hi, and welcome to week five of the Haskell course. As Larry Wall once said, one of the main virtues of the programmer is laziness. Laziness is also a key feature of Haskell. And this week, you will learn what laziness and strictness really mean. So far, we have used types in our programs but we have actually glossed over the details. And in Haskell, types are a really essential part of the language and the type system is very powerful. This week, you will learn about function types, type variables, and type classes. We will also show you Haskell can infer the types of expressions in a program so that you don’t have to provide the type information for every single expression.

0:53
Enjoy and see you next week.
```

This week, we are going to explore some of Haskell’s features that make it so distinctive, as a programming language. These include laziness, infinite data structures and type inference.

We will also review Haskell deployments in the real world, when we meet Katie Ots from Facebook.

### Lazy is Good

```
0:04
JEREMY: In real life, laziness is frowned upon and considered to be a bad thing. However, in programming languages laziness is a feature and may be considered a good thing. Haskell is a lazy language. This means that the evaluation of expressions is delayed until their values are actually needed. The opposite is eager evaluation, which is what most programming languages implement, like C, Java, and Python. For instance, consider this expression– f applied to 1 plus 1, given this function definition f x equals 0. In an eager language, the calculation 1 plus 1 is done when the function f is invoked. This is call by value.

0:59
Whereas in a lazy language, like Haskell, the calculation 1 plus 1 is only done when the parameter value is used in the function body, known as call by need. So in a lazy language, if a parameter value is never needed then the parameter is never evaluated. Consider f x y equals y, then f 1 add 1 2 add 2 has value 4 and the calculation of 1 add 1 is never performed. Formally, we say the function f is strict in its second argument. Some values do not terminate when we try to evaluate them. The simplest non-terminating value is called bottom, written in mathematical notation as shown here. Its recursive definition in Haskell is bot equals bot.

1:58
A function is strict in its argument if when we supply bottom as that argument the function fails to terminate.

2:08
F bot 42 terminates fine since we never evaluate the first argument. On the other hand, f 42 bot loops forever, or at least until we press Ctrl-C in the GHC interactive console.

2:23
Infinite data structures. Laziness is very useful when dealing with infinite data. For example, think of the infinite list of one values. That’s an infinite list where each element is the integer one. We can define this in GHCi as follows. Let ones equal 1 cons’d onto ones. See the recursive nature of the definition here. What value is returned by head ones? Simply the integer value one. What value is returned by tail ones? An infinite list. I need to press Ctrl-C to interrupt this printing and evaluation of the expression. The same is true if I just try to evaluate the whole expression ones. I get another infinite list of one values.

3:21
The take function selects a finite number of elements from the front of a potentially infinite list. So let’s say take 3 ones and I get that, the finite list of 3 one values. The drop function drops elements from the front of the list and returns the rest of the list. Again, it’s infinite so I’m going to press Ctrl-C to interrupt the evaluation. In summary, if computations are not needed, then they won’t be evaluated. And we can compute with infinite data structures so long as we don’t traverse the structure infinitely.
```

Laziness is a key feature of the Haskell programming language. The main idea is that we only evaluate expressions when they are actually needed. Unlike Haskell, most programming languages implement eager evaluation.

### Infinite Data Structures

In the previous video, we looked at infinite lists. We can define an infinite list of consecutive integers as follows:

```hs
[1..]
```

We can evaluate this list, but it won’t print out in its entirety — because it goes on for ever. To repeat a set of identical values, use the repeat function.

```hs
repeat 'a'
```

Again, this list is infinite. Use the take and drop functions to deal with infinite lists. It’s permitted to do a finite amount of processing in an infinite list, but not to traverse it infinitely.

The reason why Haskell can process infinite lists is because it evaluates the lists in a lazy fashion — i.e. it only evaluates list elements as they are needed.

Now let’s have a look at two well-known integer lists. We will study their recursive definitions.

#### Fibonacci Numbers

The nth Fibonacci number is the sum of the previous two Fibonacci numbers. The first two numbers are both 1. Then the third is 2, followed by 3, 5, etc.

```hs
1, 1, 2, 3, 5, 8, 13, 21, ...
```

This can be expressed in Haskell using the zipWith function, combining pairs of elements with the addition operator.

```hs
let fibs = 1:1:(zipWith (+) fibs (tail fibs))
```

We can evaluate individual elements of this list using the !! indexed list selection. Or we could take the first n elements of the fibs list.

#### Prime Numbers

Below is a series of `filter` expressions to calculate an infinite list of prime numbers.

```hs
properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..]
```

Let’s analyse how this definition works:

1. The `properfactors` function takes an integer value x and returns a list of proper factors for x. Factors are numbers that divide x and leave no remainder. Proper factors for an integer x do not include 1 or x.
2. The `numproperfactors` function simply counts how many proper factors there are for x, by returning the length of the `properfactors` x list.
3. Finally, primes list uses the `filter` function to select integers x that have no factors in the range 2 to (x-1) inclusive.

### To Infinity (but not beyond)

#### Question 1

How do we generate an infinite list of integer 1 values?

- [ ] `[1..]`
- [ ] `[1..1]`
- [x] `repeat 1`
- [ ] `take 1`

#### Question 2

Which one of the following functions will *not* loop infinitely, if we evaluate it in ghci?

- [ ] `length [1..]`
- [ ] `tail [1..]`
- [x] `take 10 [1..]`

#### Question 3

Given a Tree data type as defined earlier in the course:

```hs
data Tree = Leaf | Node Int Tree Tree deriving Show
```

with `Leaf` and `Node` constructors, then how do we define an infinite tree?

- [ ] `mkInfiniteTree = mkInfiniteTree Node 0`
- [x] `mkInfiniteTree = Node 0 (mkInfiniteTree) (mkInfiniteTree)`
- [ ] `mkInfiniteTree = Node 0 .. (mkInfiniteTree)`

#### Question 4

Which one of the following expressions generates an infinite list of successive *factorial* numbers? (Recall that the nth factorial is the product of the first n positive integers.)

- [x] `facts = map (\x-> (foldr (*) 1 [1..x])) [1..]`
- [ ] `facts = [1,2,6,...]`
- [ ] `facts = (*) [1..]`

#### Question 5

Does the following expression terminate?

```hs
let bot = bot
    bottomList = repeat bot
in length(take 5 bottomList)
```

- [x] yes, returning integer value 5
- [ ] no, it loops forever

## More about Types

Types, lambda functions and type classes

### Type Horror Stories

Jeremy says

remember a student who was writing a Game of Thrones ‘top trumps’ computer program in Python. Users entered values into the system interactively, so ‘Tyrion’ had a Wisdom ranking of 95 and ‘Daenerys’ had a Wealth ranking of 70, etc.

However we discovered that the program had a subtle bug. When we compared characters on the same ranking, a character with a score of ‘99’ would beat a character with a score of ‘100’. It took me ages to work out why…

The problem was that the Python program represented these ranking values as Strings, and

```py
"99" > "100"
```

in terms of String comparisons. We needed to use the `int()` function to convert the Strings to integer values.

Have you ever written a program in an untyped or weakly typed language and got things badly wrong? Would a stronger type system have helped you? Please share your stories about type problems in this discussion.

### Types, lambda functions and type classes

#### Function Types

- Ordinary data types are for primitive data (like *`Int`* and *`Char`*) and basic data structures (like *`[Int]`* and *`[Char]`*).
- Algebraic data types are types that combine other types either as records ('products'), e.g.

```hs
data Pair = Pair Int Double
```

or as *variants* ('sums'), e.g.

```hs
data Bool = False | True
```

- Functions have types containing an arrow, e.g. *`Int -> String`*.
- We now look at function types in more detail.

#### Lambda expressions

- Lambda expressions (named after the greek letter) play a very important role in functional programming in general and Haskell in particular.

##### Named and anonymous expressions

- You can give a name *`sum`* to an expression `2+2`:
```hs
    sum = 2+2
```

- But you can also write *anonymous expressions* -- expressions that just appear, but are not given names.
```hs
    (-b) + sqrt (b^2 - 4*a*c)
```

- Without anonymous expressions, writing this would almost be like assembly language:
```hs
    e1 = (-b)
    e2 = b^2
    e3 = 4*a
    e4 = e3*c
    e5 = e2-e4
    e6 = sqrt e5
    e7 = e1+e6
```

###### Some background

- Sometimes in a mathematics or physics book, there are statements like "the function `x^2` is continuous..."
- This is ok when the context makes it clear what `x` is.
- But it can lead to problems. What does `x*y` mean?
    - Is it a constant, because both `x` and `y` have fixed values?
    - Is it a function of `x`, with a fixed value of `y`?
    - Is it a function of `y`, with a fixed value of `x`?
    - Is it a function of both `x` and `y`?
- In mathematical logic (and computer programming), we need to be precise about this!
- A lambda expression `\x -> e` contains
    - An explicit statement that the formal parameter is `x`, and
    - the expression `e` that defined the value of the function

###### Anonymous functions

- A function can be defined and given a name using an equation:
```hs
    f :: Int -> Int
    f x = x+1
```

- Since functions are "first class", they are ubiquitous, and it's often useful to denote a function anonymously.
- This is done using *lambda* expressions.
```hs
    \x -> x+1
```

###### Pronounced "lambda x arrow x+1".

There may be any number of arguments:
```hs
\x y z -> 2*x + y*z
```

###### Using a lambda expression

Functions are first class: you can use a lambda expression wherever a function is needed. Thus

```hs
f = \x -> x+1
```

is equivalent to

```hs
f x = x+1
```

But lambda expressions are most useful when they appear inside larger expressions.

```hs
map (\x -> 2*x + 1) xs
```

##### Monomorphic and polymorphic functions

###### Monomorphic functions

Monomorphic means "having one form".

```hs
f :: Int -> Char
f i = "abcdefghijklmnopqrstuvwxyz" !! i

x :: Int
x = 3

f :: Char->String
f x = x:" There is a kind of character in thy life"
```

###### Polymorphic functions

Polymorphic means "having many forms".

```hs
fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b
```

##### Currying

- Most programming languages allow functions to have any number of arguments.
- But this turns out to be unnecessary: we can restrict all functions to have just one argument, *without losing any expressiveness*.
- This process is called *Currying*, in honor of Haskell Curry.
    - The technique makes essential use of higher order functions.
    - It has many advantages, both practical and theoretical.

### Curry is on the menu
```
0:07
WIM: Hello, everyone. In this short video, we are going to introduce two new concepts that are slightly more advanced. And they are currying and partial application. So consider the function signature like this, where we have a type X, Y, Z, and return value of type A. So this is a function that takes three arguments of three different types and produces a return value of type A. However, the arrow here in this type signature is meaningful. And also, it has a certain associativity. In fact, it associates to the right.

0:55
And that means that, actually, we can write this expression also like this, which means that we actually can consider f as a function that takes an element of type X and returns a function of Y to function of Z to A. Similar, this return function, Y, can be considered both as a function of Y and Z to A or as a function of Y to Z to A. So this idea that you can always rewrite the function of a single argument returning another function is called currying. To illustrate this on an actual function, it’s easiest to use lambda functions. So suppose we have a lambda function of X, Y, and Z, and function body is completely irrelevant.

1:50
Then to rewrite this, so that this becomes a function of a single argument. We create a new lambda function, which is this one. So the body remains completely untouched. And we can rewrite this inner function once more.

2:09
And in this way, we have created a function of a single argument which returns a function of a single argument which returns a function of a single argument, which eventually returns the value. So this is the typical technique known as currying you use to transform multi-argument functions into single argument functions. So it’s named after the logician Haskell Curry. But actually, he wasn’t the first to invent this. The first one to invent this technique was called Moses Schonfinkel, another logician. But well, his name wasn’t so catchy, so they decided to go with currying. Another concept closely related to currying is that of partial application.

2:52
For example, consider the function sq, which takes X and Y and returns the sum of the squares.

3:05
This function can actually, again, be slightly rewritten as– because the function application, actually, is left associative. That means that what we have here is a function in its own right that operates on Y. So we can do other things like, for instance, say OK. Let’s define sq4 as sq4. And now sq4 is a new function which just takes one argument. If we now call Sq4 on value three, it will return 25. This is an example of partial application. So basically, we’ve applied four to the function argument x, but we haven’t applied anything to y, and the result is a new function. And then we can use this new function for doing our computation.

4:06
So this technique of partial application is really used a lot in functional programming in Haskell, and it’s the reason why you can write things, for instance, like this.

4:20
So here, the times two is partial application of the function multiplication that we use as an operator here, so it takes two arguments and returns an argument. We have applied it partially, using the two. We get a new function, which we can use in the map, because map requires functions with a single argument. And so we have a function that doubles the elements of a list.

4:46
So this was just to give you a quick intuition on these key concepts of currying and partial application.
```

#### Partial Application and Currying

##### Currying

Consider a type signature of a function with three arguments:

```hs
f :: X -> Y -> Z -> A
```

The arrow "->" is right-associative, so this is the same as:

```hs
f :: X -> (Y -> (Z -> A))
```

What this means is that we can consider `f` as a function with a single argument of type `X` which returns a function of type `Y->Z->A`.

The technique of rewriting a function of multiple arguments into a sequence of functions with a single argument is called *currying*. We can illustrate this best using a lambda function:

```hs
\x y z -> ...
\x -> (\y z -> ...)
\x -> (\y -> (\z -> ... ))
```

The name "currying", is a reference to logician Haskell Curry. The concept was actually proposed originally by another logician, Moses Schönfinkel, but his name was not so catchy.

##### Partial application

Partial application means that we don’t need to provide all arguments to a function. For example, given

```hs
sq x y = x*x+y*y
```

We note that function application associates to the left, so the following equivalence holds

```hs
sq x y = (sq x) y
```

We can therefore create a specialised function by partial application of x:

```hs
sq4 = sq 4 -- = \y -> 16+y*y
sq4 3 -- = (sq 4) 3 = sq 4 3 = 25
```
This is why you can write things like:

```hs
doubles = map (*2) [1 .. ]
```

### Type Inference by Example
```
0:07
WIM: Hello, everyone. In this short video, I want to give you an intuition on how type inference in Haskell works. As you know, in Haskell, you can provide type signatures to functions, but you don’t have to. And if you don’t, then the type checker will work out what the types are of the functions that you have provided. And I want to kind of give an idea of how that works, that process that the type checker uses to figure out what types your functions that you defined would actually have. So let’s just take an example of a function of x and xs, which we define like this.

0:51
So we have a function of two arguments– x and xs. And it returns the sum of the first argument and the length of the second argument. So then the question is, what is the type of this particular function? And so the answer is that the type checker will try putting very general types. So it will say this– this is a and this is b. And this is as good as it gets for me now. And so it will say f is a b, and then some c. And then it will try to infer the constraints on each of these type variables. And the more it can be constrained, the better.

1:38
So in the end, hopefully it will be so constrained that it will no longer be a type variable, but a particular concrete type. So in this particular case, we have already, from the Prelude, we have types for the plus operation and for the length function. So we know that plus is of type Num a, and that the length is of type– let’s call this b, maybe, because they don’t have to be the same. So we know that length of xs, if this has to be a valid type checking function, this will have to be of type Int. And we know, from the type signature of the addition, that the types of both arguments must be the same.

2:42
So consequently, this will also have to be of type Int. So, because the x here is the x here, we already know that our type of our first argument must be Int. For the type of our next argument, well, we know that it’s a list because we call the length function on it. But that’s really all we can tell because we don’t need to know what’s in the list to calculate its length. So the type of the arguments of the list are not important. So that means that, in the end, the type inference will say that it has found a function which takes an Int and something– a list of some arbitrary type– and it will return an Int.

3:35
And so it works. So the type checker tries to apply more and more constraints so that it can actually resolve the entire types. So for instance, if we had done something like this, then automatically, we would know that head xs would have to be also type Int. Now, we know the signature for the head function. So we know this, which means that this list would have to be of type Integer. So you see, by making the expression a little bit different, we used the element of the list, and immediately, the type inference can tell us that this has to be a list of Integers. And so, in general, that’s how the whole process works.

4:31
It works by a combination of the structure of the types and the type definitions of the basic functions in the Prelude. There’s actually a lot more to this, but we will cover this in a later lecture.
```

Type inference is the process by which Haskell ‘guesses’ the types for variables and functions, without you needing to specify these types explicitly. Many functional languages feature type inference.

There is lots of theory behind type inference — [Hindley-Milner type systems](http://dev.stephendiehl.com/fun/006_hindley_milner.html) and [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#Application:_Type_inference).

However we don’t need this level of detail. Like most car drivers, we don’t know too much about how the engine works — we just drive the car. This video gives a high-level intuition for how type inference operates, which is all we need to grasp for now.

### You are the type checker

#### Question 1

What is the type of the `head` function?

**head :: **`[a] -> a`

#### Question 2

What is the type of the `putStrLn` function?

**putStrLn :: **`String -> IO ()`

#### Question 3

Given the following type declarations:

```hs
f :: T1 -> T2
g :: T2 -> T3
```

And given that the following expression typechecks:

```hs
v :: T1
v = h f g
```

What is the type of `h`?

**h :: **`T1 -> T2`** -> **`T2 -> T3`** -> **`T1`

#### Question 4

What is the type of the following function:

```hs
\f -> f f
```

- [ ] True
- [ ] Bottom
- [x] It is not possible to type this expression correctly in Haskell

#### Question 5

Complete the following type definition to define a binary tree with the values stored only in the leaf nodes:

```hs
data Tree a = Node __ __ | Leaf __
```

**data Tree a = Node **`(Tree a)` `(Tree a)`** | Leaf **`a`

#### Question 6

What is the type of the following function (use a,b,c etc as type variables in order of occurence):

```hs
\x y -> y
```

**\x y -> y :: **`a -> b -> b`

#### Question 7

Is the following expression correctly typed?

```hs
sq :: Int -> Float
sq x = x*x
```

- [ ] Yes
- [x] No

#### Question 8

Is the following expression correctly typed?

```hs
join :: String -> [String] -> String
join str strs = foldl (++) str strs
```

- [ ] Yes
- [x] No

### Summary

Function types describe the types of arguments and return value of a function. The type of an argument can be a type variable, in which case we call the function polymorphic.

Currying means rewriting a function of more than one argument to a sequence of functions, each with a single argument.

Type classes allow to impose restrictions on polymorphic type variables. Type classes express that e.g. a type represents a number, or something that can be ordered.

Type inference is the analysis of code in order to infer its type. It works using type inference rules that generate typings based on the program text.

## Haskell in the Real World

Haskell started life as an academic project. However it is now widely used in commercial software projects.

### Haskell at Facebook
```
0:07
JEREMY: Katie, thank you for inviting me to Facebook to meet you and–

0:11
KATIE: You’re welcome.

0:12
JEREMY: –to see the sights of London. It’s great being here. I want you to tell me a little bit, please, about how you got into Haskell programming, where you found out about it, and why you like it so much.

0:23
KATIE: OK. So I learned about Haskell from a professor at my university. Like most universities, we have that one professor who’s really crazy about it.

0:32
JEREMY: Oh, right. Good.

0:33
KATIE: But unlike most people, I didn’t learn about it in official classes. The classes that included Haskell were unfortunately cut from the curriculum.

0:40
JEREMY: Oh, how sad.

0:41
KATIE: And this professor– very, very sad. This professor was so passionate, he offered to teach interested students in the library in his own time. So we had little secret sessions in the library to learn about functional programming and also logic programming. So he taught about a bit of Scheme, a bit of Prolog, and also some Haskell, and that’s how I was first exposed to it.

1:00
JEREMY: Wow, the secret functional society. That’s good, isn’t it? Haskell is used quite extensively at Facebook, isn’t it? Can you tell us a little bit about how you use Haskell in your day-to-day software development?

1:13
KATIE: Yes. I work on a system called Sigma, which is basically a rules engine for catching bad things on the site, things like spam, for example. And it’s a C++ Haskell hybrid, but it runs rules that are written in Haskell in EDSL, embedded domain-specific language, called Haxl, which is open source so people can check that out.

1:36
JEREMY: How do you spell Haxl? Sorry.

1:37
KATIE: It’s H-A-X-L.

1:38
JEREMY: OK. We’ll google it.

1:40
KATIE: Haxl.

1:40
JEREMY: Cool.

1:41
KATIE: And this system runs more than a million queries a second to fight badness on the site. So Haskell is being used in the real world in production to help make Facebook a nicer place.

1:55
JEREMY: And there are lots of people using Haskell at Facebook.

1:58
KATIE: Yeah, not just me. We have dozens of people here who are writing these rules in Haskell and also people that are working on this project, of course, as well.

2:09
JEREMY: Can you tell us a little bit about the Commercial Users of Functional Programming Conference? You’re involved with that, I think, and that’s something that showcases functional programming in what you might call the real world, I suppose.

2:23
KATIE: Yes. And it’s been going for quite a number of years now, I think over a decade. So this is run as part of ICFP, the International Conference on Functional Programming, and I’ve been co-chairing it for this year and the previous year. And we’ve got some great talks coming up, so if anyone wants to head to Japan.

2:43
JEREMY: A nice holiday. A nice functional holiday in Japan. Very good. OK.

2:47
KATIE: But it’s at the stage now where it’s no longer a niche thing. I think when CUFP started it was people just finding camaraderie in other people having the frustrating experience of trying to get FP into their workplace and trying to make it work and convince their employers. I think we’re well past that now.

3:06
JEREMY: So kind of the battle is almost half won, you’re saying already.

3:10
KATIE: Well, certainly there are lots of big companies like Facebook that people can point to to say, well, why can’t we use it at my company? Facebook’s using it. And there are other big names. So a lot of that battle is won now. And the event has changed focus as a result of that.

3:26
JEREMY: On our course, Katie, we’re learning about monads. We’ve looked at Maybe and the IO monad so far. I’d like you to try and convince that monads aren’t scary.

3:39
KATIE: Oh, OK. So why do people even think monads are scary, right? Maybe, firstly, because that have an unfamiliar name– monad. People probably haven’t heard that word before, sure. And also because there’s this bit of a cottage industry of writing monad tutorials on the internet, and people think, oh, if it needs all these tutorials, it must be hard. Not true, not true at all. All monads are are basically a pattern. It’s the simplest way of explaining what it is.

4:03
JEREMY: OK.

4:04
KATIE: Monads are useful, because we believe in the DRY principle as good programmers– Don’t Repeat Yourself. And monads are simply a way of not repeating yourself. It happens to be that there are many quite disparate things actually, as disparate as Maybe and IO and Lists and a number of other things–

4:23
JEREMY: But we’ll come to that next week.

4:25
KATIE: –yes, yes– that fit a particular pattern. And that means that you can write code once for all the things that fit that pattern and not repeat yourself. And that’s basically it in a nutshell. And monad, of course, is only one of these abstractions that we have in Haskell. There are many others too.

4:38
JEREMY: OK.

4:39
KATIE: And they’re useful. And that’s it. That’s really all there is to it.

4:42
JEREMY: I feel slightly reassured then. Thank you. OK.

4:44
KATIE: Glad I could be of service.

4:46
JEREMY: We’re going to go into a lot more detail next week. But you’ve got a great video about monads as superheroes online. Can you ever give us a two-minute pitch of that, and we’ll post a YouTube link on our website below.

4:58
KATIE: OK, sure. First, I should clarify when I say monads are superheroes, I’m not trying to add another analogy–

5:03
JEREMY: Oh, right.

5:04
KATIE: –to all the analogies you already see out there about monads, because I believe it’s better to go from the concrete and see lots of examples and then build up your own intuition of what the abstraction means. So you’re not going to know what monads are just by hearing that they’re like superheroes. But in the talk, I basically just talk about what the key aspects of a monad are. So monads are basically– there’s a type class. I understand you’ve already taught that.

5:29
JEREMY: Yes.

5:29
KATIE: So you can say this.

5:30
JEREMY: Yes.

5:30
KATIE: A type class that has two– there’s another way to define it with three– but two or three key functions on it. And it needs to follow some laws, the way that you implement this. That’s really the crux of it.

5:42
JEREMY: OK. More next week.

5:43
KATIE: Yes, sure. I’m sure you’ll go into all the details.

5:46
JEREMY: We will, yes, excruciatingly. Thank you. Good. If someone on our course came along and said, right, will functional programming make me more employable? What would you say?

6:02
KATIE: This is a good question. I think that a lot of employers out there are starting to twig to the fact that people who are interested enough in programming to learn something that’s not the mainstream paradigm like functional programming tend to be good-quality programmers. So I think, in general, it serves you well to have those skills as part of your toolset. There aren’t as many purely functional programming jobs out there as there are other jobs, for sure, but they do exist. And it seems that their numbers are growing. So, yeah, I think it’s definitely a benefit to have those skills.

6:37
Most importantly, though, I think it’s great to learn functional programming just to learn how to think in a different way about problems.

6:43
JEREMY: Yeah. Good. We’ve been trying to emphasize that on our course. Thinking functionally is a good skill to have, even if you don’t use functional languages. Katie, tell us about the big gong that’s next to you. What’s this for?

6:55
KATIE: So this is a tradition here, that when you ship something, you get to come and hit the gong so the whole office can hear that something’s been shipped.

7:03
JEREMY: It’s quite loud.

7:04
KATIE: It is. Do you want me to demonstrate?

7:08
JEREMY: Well, why don’t we ship our interview, and you can whack the gong to tell everyone we’ve done it.

7:13
KATIE: All right. I don’t know how I’m going to explain this one, but sure.
```

**Jeremy recently visited Facebook’s London office, where he met Katie Ots. She is a software engineer at Facebook, developing Haskell code for real-world applications.**

Katie explained how and why Haskell is used at Facebook. She is also well-known for her talk on _superhero monads_. We will explore monads in more detail next week, but you might also want to check out her talk before then.

- [Superhero monads on youtube](https://youtu.be/MlZCiiKGbb0)
- [Slides for Katie’s talk](http://monads.codemiller.com/#/)

### Haskell in the Wild

**Functional programming (particularly Haskell) is used by a growing number of companies today, including some large multinationals.**

#### Companies that use Haskell

There are lists of companies that use Haskell on [the Haskell web site](https://wiki.haskell.org/Haskell_in_industry) and on [Quora](https://www.quora.com/Where-is-Haskell-used-in-industry-today-2015). A few highlights are Facebook, IBM, Twitter, AT&T, Bank of America, Barclays Capital, NVIDIA and Microsoft.

Some interesting links are:

- Facebook uses Haskell in several projects, for example [Fighting spam with Haskell](https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/).
- [Galois](https://galois.com/) has been an active Haskell user and promoter for over a decade. [This talk](https://www.scribd.com/document/45049621/Building-a-business-with-Haskell-Case-Studies-Cryptol-HaLVM-and-Copilot) introduces three case studies from some of their projects.
- The New York Times has a nice talk about [Haskell in the newsroom](https://www.infoq.com/presentations/haskell-newsroom-nyt).
- [Running a Startup on Haskell](https://www.infoq.com/presentations/Running-a-Startup-on-Haskell).

#### Companies that use functional programming languages other than Haskell

- Twitter uses [Scala](http://www.scala-lang.org/), an FP with Java syntax running on the JVM. [How and why Twitter uses Scala](https://www.redfin.com/blog/2010/05/how_and_why_twitter_uses_scala.html).
- Walmart uses [Clojure](http://clojure.org/), a dialect of the Lisp programming language. [Walmart runs Clojure at scale](http://blog.cognitect.com/blog/2015/6/30/walmart-runs-clojure-at-scale).
- Bloomberg uses [OCaml](https://ocaml.org/), an object-oriented version of the ML programming language. [OCaml at Bloomberg](http://cufp.org/2014/maxime-ransan-adopting-functional-programming-with-ocaml-at-bloomberg-lp.html).
- SocialText uses [LiveScript](http://livescript.net/), a functional language with many similarities to Haskell, which compiles to JavaScript. [Read about their experiences](http://www.aosabook.org/en/posa/from-socialcalc-to-ethercalc.html).
