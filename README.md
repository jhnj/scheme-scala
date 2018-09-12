# Scheme-scala
Scheme interpreter written in pure functional Scala, inspired by
[Write yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
(Haskell) and
[Functional programming in Scala](https://www.manning.com/books/functional-programming-in-scala).
The interpreter implements a subset of [R5RS](https://schemers.org/Documents/Standards/R5RS/HTML/).

### Setup
The project is built using `sbt` which can be installed by following the instructions
[here](https://www.scala-sbt.org/1.x/docs/Setup.html).

### Usage
The interpreter can be run either in interactive mode (REPL) or non-interactive (on a file).
To run the REPL type `sbt run` and to run on a file type `sbt "run filename.scm"`.

### Tests
Currently the tests are quite limited, there are unit tests for the `Env`, `Eval` and `SchemeParser`
classes. Run the tests with `sbt test`.

### Future improvements
Currently the parser doesn't handle the positions of parsing errors correctly, the position is always
set to the first character even if the error is actually later in the code, e.g.:
```
(+ 1 (..))
^

[1:1] -> regex: [a-zA-Z]
```

I'd also like to improve the testing by adding at least:
unit tests for the base `Parser` itself as well as some
integration tests (scheme source files that are verified to produce the correct output).



