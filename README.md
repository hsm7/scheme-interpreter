# Scheme Interpreter
A simple Scheme interpreter written in Scala. Inspired by Peter Norvig's [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html)

Uses RegexParsers from [scala-parser-combinators](https://github.com/scala/scala-parser-combinators) to parse Scheme expressions
to the following Expression abstract syntax tree.

```scala
sealed trait Expression {
  
  // Datatype definition:
  //    Expression  = Empty | Value | Symbol | Func | Cons
  //    Empty       = Empty
  //    Value       = Integer(value: Int) | Number(value: Double) | Bool(value: Boolean) | Str(value: String)
  //    Symbol      = Symbol(s: String)
  //    Func        = Func(op: Symbol, args: Expression, f: Expression => Expression)
  //    Cons        = Cons(car: Expression, cdr: Expression)
  
}
```

### TODO
- [x] Scheme calculator
- [x] List functions
- [x] Global environment
- [x] Definition expressions
- [ ] Conditional expressions
- [ ] Procedure call expressions
- [ ] Interpreter REPL
- [ ] Quotation expressions
- [ ] Assignment expressions
- [ ] Lambda expressions
- [ ] Local environments and lexical scoping