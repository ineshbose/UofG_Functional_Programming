# ASTEmitter

## Brief

The purpose of this coursework is to create an emitter (also called a "pretty-printer") for an Abstract Syntax Tree (AST). An AST is the abstract representation of a program. In this coursework, it is an instance of an algebraic data type. Instances of the AST as well as the concrete syntax of the corresponding programs are provided. Use the AST representation to print out the concrete syntax of the program.

The key activites are to create a suitable algebraic data type that is compatible with the provided AST instance. For each of these stages, testing is required, and the marks for the assessed exercise will be based entirely on the test results.

## Aims

The aims for the coursework are to assess the following learning outcomes:

* Write simple programs involving elementary Haskell techniques, including pure function definitions;
* Define new algebraic data types and use recursion to define functions that traverse recursive types;
* Demonstrate understanding of how to express data structures and function interfaces using types, and how to infer types;
* Develop substantial software applications including system interaction;
* Construct, adapt, and analyse code using standard Haskell platform tools

## Overview

A compiler typically parses a program written in a given programming language and produces essentially a different representation of that program. Typically that is a binary for execution on a processor, but it can also be assembly or source code in another or even the same programming language. In the latter case, we call it a source-to-source compiler. The typical sequence of actions is:

1. parse the source code into an Abstract Syntax Tree
2. transform the Abstract Syntax Tree
3. emit the target code

In this coursework, the aim is to write an emitter for an AST. The AST is a recursive algebraic datatype and several instances are given. The first task is to infer the actual datatype from the instances. The second task is to write an emitter to re-create the source code of the programs from which the AST instances have been derived. The source code is also given.

The provided code skeleton must be used, along with a testbench that is strongly advised to use with it since it will be used to mark the code.

## Specification

### Target language

The target language is syntactically very similar to a restricted subset of Haskell, with the main exception that instead of the list type it has a vector type of a fixed size. It has the following primitives:

* variables which represent names of functions, function arguments and return values, or stencil definitions
* type declarations for the above, using the following types:
  * **Scalar type:** either `Float` or `Int`
  * **Vector type:** `Vec vsize vtype` where `vsize` is an integer (the size of the vector) and `vtype` can be either `Float`, `Int`, a tuple or a stencil vector type (see next)
  * **Stencil vector type:** `SVec svsize svtype` where `svsize` is an integer (the size of the stencil vector) and `svtype` can be either `Float` or `Int`
  * **FVec type:** `FVec fvbounds svtype` where `fvbounds` is an a list of tuples of integer `[(Int,Int)]` and `fvtype` can be either `Float` or `Int`
  * **Tuple type:** `(type1,type2,...)` as in Haskell
  * For example:

    ```hs
    -- Input arguments
    v 0 :: Vec 2139552 Float
    w 0 :: Vec 2139552 Float
    u 0 :: Vec 2139552 Float
    dx1 0 :: FVec [(-1,151)] Float
    dy1 0 :: FVec [(0,151)] Float
    dz1 0 :: FVec [(-1,92)] Float
    -- Output arguments
    v 3 :: Vec 2074891 Float
    w 3 :: Vec 2074891 Float
    ```

  * Functions are scalar and their types are restricted to the following pattern:

    ```hs
    f :: arg or argtup -> res or restup
    f :: nm arg or argtup -> arg or argtup -> res or restup

    -- Here arg or argtup, res or restup and nm arg or argtup can be a Scalar, SVec, FVec or a tuple of these, so not Vec.
    -- Function type declarations
    f t1 :: (Float, SVec 5 Float ) -> Float
    f 1 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float,FVec [(-1,92)] Float)
        -> (SVec 5 Float,SVec 5 Float,SVec 4 Float) -> (Float,Float,Float)
    f 2 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float)
        -> (SVec 2 Float,SVec 3 Float,SVec 2 Float) -> Float
    f t2 :: (Float, SVec 2 Float ) -> Float
    f 3 :: Float -> Float

    ```

  * the following higher-order functions (*k* and *sk* are integer values):

    ```hs
    map :: (a -> b) -> Vec k a -> Vec k b
    zipt :: (Vec k a,Vec k b,...) -> Vec k (a,b,...)
    unzipt :: Vec k (a,b,...) -> (Vec k a,Vec k b,...)
    stencil :: SVec sk Int -> Vec k a -> Vec k (SVec sk a))
    ```

  * tuples, using the same syntax as Haskell, e.g. `(v1,v2,...)`
  * a main function with the fixed structure of a Haskell let-expression

    ```hs
    main (...) = let
        v1 = ...
        v2 = ...
    in
        ...
    ```

  * let-bindings, i.e. assignments of expressions to variables or tuples, for example

    ```hs
    v s 0 = stencil s1 v 0
    w t 1 = map f t1 (zipt (w 0,v s 0))
    (u 1,v 1,w 1) = unzipt (map (f 1 (dx1 0,dy1 0,dz1 0)) (zipt (u 0,v s 0,w t 1)))
    ```

  * stencil definitions which are simply lists of integers bound to variables:

    ```hs
    s3 = [305,23409,23560,23561,23712]
    s4 = [23409,46665]
    ```

A minimal complete program in this language is for example

```hs
v in :: Vec 16 Float
v out :: Vec 16 Float

main :: Vec 16 Float -> Vec 16 Float
main v in = let
    v out = v in
in
    v out
```

More examples are given in the [`ASTInstances.hs`](./src/ASTInstances.hs) source file.

### AST type and inference

The AST type is defined as

```hs
type AST = [(Expr,Expr)]
```

where `Expr` is a recursive algebraic datatype that needs to be inferred from the example instances in `ASTInstances.hs`. A partial definition in `AST.hs` has been provided, and needs to be completed. For example, the AST instances for the minimal example from above looks like:

```hs
ast1 = [
    (
        Vec VO (Scalar VDC DFloat "v out"),
        Vec VI (Scalar VDC DFloat "v in")
    )
]
```

To illustrate the process of inference of `Expr` from this example: see that a valid variant of the `Expr` type is `Vec _ _`. The source [`AST.hs`](./src/AST.hs) contains all auxiliary type definitions, and from that see that `V0`, `VI` and `VDC` are variants of the sum type `VE` and that `DFloat` is a variant of the sum type `DType`. So the full `Scalar` type can be inferred as `Scalar VE DType String`. The types for all variants of `Expr` that represent variables and scalar functions are also provided, so it can be seen that `Scalar` is a variant of `Expr`, and thus the full `Vec` type is `Vec VE Expr`.

A similar inference for the type for the types of the higher-order functions `map`, `zipt`, `unzipt` and `stencil` needs to be done.

### Additional types

Apart from the AST, which represents the body of the main function, the `ASTInstances.hs` source file also contains for every program the following:

* **Stencil definitions:** `type StencilDefinition = (String,[Integer])`
* **Main argument declarations:** `type MainArgDecls = ([(String,DType)],[(String,DType)])`
* **Scalar function signatures:** `type FunctionSignature = (String,[Expr])`

These datastructures represent the additional defintions and declarations in the program outside of the main function. These are also defined in [`AST.hs`](./src/AST.hs).

### Emitter

The emitter must be implemented in [`ASTEmitter.hs`](./src/ASTEmitter.hs) and must use the provided functions:

For the type declaration and stencil definition parts, only the functions `ppStencilDef`, `ppArgDecl` and `ppFSig` have to be implemented. These functions emit resp. the stencil definitions, the main function argument type declarations and the scalar function type declarations.

For the main program, only two functions `ppLHSExpr` and `ppRHSExpr` have to be implemented. These two functions emit the left-hand side (LHS) and right-hand side(RHS) of all possible let-bindings. `ppExprTup` combines them to emit the actual binding and `ppAST` and `ppBindings` combine all pretty-printed bindings into the body of the main function. All other functions are already provided.

## Part I: Explain the design

The aim of the first part of the assignment is to think carefully about the solution, before writing any code.

### What to submit for Part I

For the first part of the assignment, explain in no more than a single page how to create a pretty-printer for the AST instance. The main criteria for this explanation is that it is correct, complete, clear and readable. The explanation should be in words, but pseudo-code or Haskell code can be used to illustrate further. This document must be submitted in PDF format through the Moodle submission system.

### Criteria and weighting

The document does contribute to the final mark, but not as a separate mark: it must be submitted and it must be of sufficient quality, otherwise 10 marks will be penalised (out of 60) on the total for the assignment.

The quality criteria for the document are:

* 4/10 correctness,
* 3/10 completeness,
* 2/10 clarity,
* 1/10 readability

## Part II: Implement and test the design

### What to submit for Part II

For the second part of the assignment, the code for the AST datatype and the pretty printer has to be written.

* Start from the provided code and only add own code in `ASTEmitter.hs` and `AST.hs`
* More tests can be added in `ASTInstance.hs` and/or `ASTEmitterTests.hs`
* Do not modify or remove any other part of the provided code
* Submit this code in a *gzipped tar archive*.

### What is provided

A code skeleton and testbench is provided as a valid *stack* project on GitHub: <https://github.com/wimvanderbauwhede/fph/>

### How to test the code

A testbench will be provided as part of the stack code skeleton which will be used to test the code. Besides the provided tests, additional tests will test more details of the implementation.

It is strongly recommended to write additional tests. To run the emitter on the provided example AST instance, run

```sh
stack run
```

To test the code, run

```sh
stack test
```

### Marking

The code will be marked based on the test performance only. Code build and execution will be checked for correctness and then the testbench will be run on it.

The coursework is marked out of 60, the marking scheme is as follows:

* Code type checks: 5/60
* Code compiles and runs: 5/60
* Code passes tests: 50/60
* As explained in Part I, a failed submission of Part I results in a -10 penalty
* Non-adherence to the formal submission requirements carried a 2-band penalty as per the School policy
