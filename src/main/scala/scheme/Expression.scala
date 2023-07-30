package scheme

import scheme.Interpreter.EvaluateError

/* Scheme Expression API */
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

/* ADT representing Scheme expressions abstract syntax tree */
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

// Represents an empty expression.
case object Empty extends Expression {
  override def print: String = "()"
  override def printAST: String = print
  override def evaluate: Expression = this
}

// Represents a Scheme list expression.
case class Cons(car: Expression, cdr: Expression) extends Expression {
  override def print: String = cdr match {
    case Empty => "" + car
    case _ => car + " " + cdr.print
  }

  override def toString: String = "(" + print + ")"
  override def printAST: String = "List(" + car.printAST + ", " + cdr.printAST + ")"
  override def evaluate: Expression = Cons(car.evaluate, cdr.evaluate)
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
  override def evaluate: Expression = f(args)
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

/**
 * Scheme primitive values are represented as an abstract class and multiple case classes
 */
sealed abstract class Value extends Expression {
  override def evaluate: Expression = this
}

object Value {
  def apply(n: Double): Number = Number(n)
  def apply(n: Int): Integer = Integer(n)
  def apply(n: Boolean): Bool = Bool(n)
  def apply(s: String): Str = Str(s)
}

/**
 * Represents a Scheme string expression
 * @param value String value of Scheme string
 */
case class Str(value: String) extends Value {
  override def print: String = value
  override def printAST: String = "Str(" + value + ")"
}

/**
 * Represents a Scheme integer expression
 * @param value Int value of Scheme integer
 */
case class Integer(value: Int) extends Value {
  def + (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value + b)
    case Number(b) => Value(value + b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def - (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value - b)
    case Number(b) => Value(value - b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def * (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value * b)
    case Number(b) => Value(value * b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def / (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value / b)
    case Number(b) => Value(value / b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  override def print: String = value.toString
  override def printAST: String = "Int(" + value + ")"
}

/**
 * Represents a Scheme number expression
 * @param value Double value of Scheme number
 */
case class Number(value: Double) extends Value {
  def + (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value + b)
    case Number(b) => Value(value + b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def - (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value - b)
    case Number(b) => Value(value - b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def * (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value * b)
    case Number(b) => Value(value * b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def / (expr: Expression) : Expression = expr match {
    case Integer(b) => Value(value / b)
    case Number(b) => Value(value / b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  override def print: String = value.toString
  override def printAST: String = "Num(" + value + ")"
}

/**
 * Represents a Scheme boolean expression
 * @param value Boolean value of Scheme boolean
 */
case class Bool(value: Boolean) extends Value {
  override def print: String = if (value) "#t" else "#f"
  override def printAST: String = "Bool(" + value + ")"
}
