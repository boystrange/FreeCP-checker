# FreeCP

This is a proof-of-concept implementation of the light type system for FreeCP, a
process calculus based on classical linear logic similar to CP that supports
context-free session types and polymorphic process recursion.

## To compile and run

* Run `make` to compile the source code.
* Run `make check` to run FreeCP against the examples.

## Source Code

* [Atoms](src/Atoms.hs): basic data types for representing identifiers
* [Checker](src/Checker.hs): type checker and measure constraint generation
* [Common](src/Common.hs): general utility functions not found in
  Haskell's standard library
* [Exceptions](src/Exceptions.hs): definition and pretty printing of
  exceptions
* [Instrumenter](src/Instrumenter.hs): type and process
  instrumentation with measure transfer operations
* [Lexer](src/Lexer.x): lexical analyser
* [Main](src/Main.hs): parsing of command-line options and main module
* [Measure](src/Measure.hs): definition of measures and related operations
* [Parser](src/Parser.y): syntax analyser
* [Process](src/Process.hs): representation of processes
* [Render](src/Render.hs): pretty printing of types and processes
* [Solver](src/Solver.hs): solver for measure constraints (wrapper
  for [Limp](https://hackage.haskell.org/package/limp))
* [Resolver](src/Resolver.hs): type name resolution in processes
* [Type](src/Type.hs): representation of types

## Well-Typed Regular Processes

* [Boolean representation and basic operations](examples/bool.cp)
* [Natural number representation and basic operations](examples/nat.cp)
* [Syntactic sugar for free channel output](examples/forwarder.cp)
* [Non-terminating well-typed process](examples/infinite.cp)

## Well-Typed Context-Free Processes

* [Client-server interaction with matching number of messages](examples/server.cp)
* [Non-uniform stack with matching push/pop operations](examples/stack.cp)
* [Binary tree serialisation](examples/tree.cp)

## Ill-Typed Processes

* [Limitations of the light type system (Alice-Bob-Carol example)](errors/alice-bob-carol.cp)
* Examples of non-contractive process definitions
  [1](errors/non-contractive-1.cp),
  [2](errors/non-contractive-2.cp), [3](errors/non-contractive-3.cp)
* [Client-server interaction with non-terminating client](errors/server.cp)
* [Output operation mismatch](errors/tag.cp)
* [Linearity violation](errors/unused-channel.cp)
