# Scheme Interpreter
![CI Build](https://github.com/hsm7/scheme-interpreter/actions/workflows/sbt.yml/badge.svg)

A simple Scheme interpreter written in Scala. Inspired by Peter Norvig's [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html)

Uses RegexParsers from [scala-parser-combinators](https://github.com/scala/scala-parser-combinators) to parse Scheme expressions
to the following Expression abstract syntax tree.

```scala
sealed trait Expression {
  
  // Datatype definition:
  //    Expression  = Empty | Value | Symbol | Procedure | Lambda | Cons
  //    Empty       = Empty
  //    Value       = Integer(value: Int) | Number(value: Double) | Bool(value: Boolean) | Str(value: String)
  //    Symbol      = Symbol(s: String)
  //    Procedure   = Procedure(op: Symbol, args: Expression)
  //    Lambda      = Lambda(params: Expression, f: Expression => Expression)
  //    Cons        = Cons(car: Expression, cdr: Expression)
  
}
```

### TODO
- [x] Scheme calculator
- [x] List functions
- [x] Global environment
- [x] Definition expressions
- [x] Conditional expressions
- [x] Procedure call expressions
- [ ] Interpreter REPL
- [ ] Quotation expressions
- [ ] Assignment expressions
- [x] Lambda expressions
- [ ] Local environments and lexical scoping