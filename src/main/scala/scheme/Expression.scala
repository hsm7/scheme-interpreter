package scheme

import scheme.Interpreter.EvaluateError

/** Scheme Expression API */
object Expression {

  /**
   * Parse Scheme expression from a string input.
   * @param program input to parse
   * @return expression AST for input Scheme `program`
   */
  def parse(program: String): Expression = Parser.parse(program)
  /**
   * Evaluate Scheme expression.
   * @param expression scheme expression to evaluate
   * @return evaluated Scheme expression for input Scheme `expression`
   */
  def evaluate(expression: Expression): Expression = expression.evaluate

}

/* Expression ADT represents Scheme expressions abstract syntax tree */
sealed trait Expression {
  // Datatype definition:
  //    Expression  = Empty | Value | Symbol | Func | Scheme
  //    Empty       = Empty
  //    Value       = Integer(value: Int) | Number(value: Double) | Bool(value: Boolean) | Str(value: String)
  //    Symbol      = Symbol(s: String)
  //    Func        = Func(op: Symbol, args: Expression, f: Expression => Expression)
  //    Cons        = Cons(car: Expression, cdr: Expression)

  /* Evaluate this Scheme expression. */
  def evaluate: Expression
  /* Partial String representation of this Scheme expression. Used to construct `toString` */
  def print: String
  /* String representation of abstract syntax tree for this Scheme expression. */
  def printAST: String
  /* Parsable and valid Scheme String representation of this expression, except for empty expression. */
  override def toString: String = print
}

/** Represents the empty expression. */
case object Empty extends Expression {
  def apply(): Empty.type = Empty
  override def print: String = "()"
  override def printAST: String = print
  override def evaluate: Expression = this
}

/** Represents a Scheme list expression. */
case class Cons(car: Expression, cdr: Expression) extends Expression {
  override def print: String = cdr match {
    case Empty => "" + car
    case _ => car + " " + cdr.print
  }

  override def toString: String = "(" + print + ")"
  override def printAST: String = "List(" + car.printAST + ", " + cdr.printAST + ")"
  override def evaluate: Expression = Cons(car.evaluate, cdr.evaluate)
}
object Cons {
  def apply(car: Expression, cdr: Expression): Cons = new Cons(car, cdr)
}

/**
 * Represents a Scheme function expression.
 * @param op function operation symbol
 * @param args function arguments expression
 * @param f a function maps input expression to output expression
 */
case class Func(op: Symbol, args: Expression, f: Expression => Expression) extends Expression {
  override def print: String = op + " " + args.print
  override def toString: String = "(" + print + ")"
  override def printAST: String = "Func(" + op.printAST + ", " + args.printAST + ")"
  override def evaluate: Expression = f(args.evaluate)
}
object Func {
  def apply(symbol: Symbol, args: Expression, f: Expression => Expression): Func = new Func(symbol, args, f)
}

/**
 * Represents a Scheme symbol expression.
 * @param symbol Scheme Symbol represented as a String
 */
case class Symbol(symbol: String) extends Expression {
  override def print: String = symbol
  override def printAST: String = "Symbol(" + symbol + ")"
  override def evaluate: Expression = this
}
object Symbol {
  def apply(symbol: String): Symbol = new Symbol(symbol)
}

/**
 * Scheme primitive values are represented as an abstract class and multiple case classes
 */
sealed abstract class Value extends Expression {
  override def evaluate: Expression = this
}

/**
 * Represents a Scheme string expression
 * @param value String value of Scheme string
 */
case class Str(value: String) extends Value {
  override def print: String = value
  override def printAST: String = "Str(" + value + ")"
}
object Str {
  def apply(s: String): Str = new Str(s)
}

/**
 * Represents a Scheme integer expression
 * @param value Int value of Scheme integer
 */
case class Integer(value: Int) extends Value {
  def + (expr: Expression) : Expression = expr match {
    case Integer(b) => Integer(value + b)
    case Number(b) => Number(value + b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def - (expr: Expression) : Expression = expr match {
    case Integer(b) => Integer(value - b)
    case Number(b) => Number(value - b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def * (expr: Expression) : Expression = expr match {
    case Integer(b) => Integer(value * b)
    case Number(b) => Number(value * b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def / (expr: Expression) : Expression = expr match {
    case Integer(b) => Integer(value / b)
    case Number(b) => Number(value / b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  override def print: String = value.toString
  override def printAST: String = "Int(" + value + ")"
}
object Integer {
  def apply(n: Int): Integer = new Integer(n)
  def from(s: String): Integer = Integer(s.toInt)
}

/**
 * Represents a Scheme number expression
 * @param value Double value of Scheme number
 */
case class Number(value: Double) extends Value {
  def + (expr: Expression) : Expression = expr match {
    case Integer(b) => Number(value + b)
    case Number(b) => Number(value + b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def - (expr: Expression) : Expression = expr match {
    case Integer(b) => Number(value - b)
    case Number(b) => Number(value - b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def * (expr: Expression) : Expression = expr match {
    case Integer(b) => Number(value * b)
    case Number(b) => Number(value * b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def / (expr: Expression) : Expression = expr match {
    case Integer(b) => Number(value / b)
    case Number(b) => Number(value / b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  override def print: String = value.toString
  override def printAST: String = "Num(" + value + ")"
}
object Number {
  def apply(n: Double): Number = new Number(n)
  def from(s: String): Number = Number(s.toDouble)
}

/**
 * Represents a Scheme boolean expression
 * @param value Boolean value of Scheme boolean
 */
case class Bool(value: Boolean) extends Value {
  override def print: String = if (value) "#t" else "#f"
  override def printAST: String = "Bool(" + value + ")"
}
object Bool {
  def apply(n: Boolean): Bool = new Bool(n)
}