# ASTEmitter

* To build:

	stack build

* To test:

	stack test

* Read the specification on Moodle for more details.
* ASTInstances.hs contains 10 instances. I will use the same instances for testing, so there are no hidden tests, but I will change the names of the identifiers and values of the constants.

The files to be changed are `AST.hs` and `ASTEmitter.hs`. You can also add your own test cases to `ASTInstances.hs` if you like. None of the other files should be modified.

To run the emitter on the provided example AST instance, run

    stack run

To test your code, run

    stack test
