package scheme


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
  def evaluate(expression: Expression)(implicit env: Environment): Expression = expression.preprocess.evaluate.simplify

}

class EvaluateError(msg: String = "Invalid expression") extends RuntimeException(msg)

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
  def evaluate(implicit env: Environment): Expression = this
  /* Parse Scheme procedure expressions from list expression. */
  def preprocess: Expression = this
  /* Simplify this Scheme expression. Removes Empty expressions after evaluating
   * define expressions */
  def simplify: Expression = this
  /* String representation of abstract syntax tree for this Scheme expression. */
  def printAST: String
  /* Parsable and valid Scheme String representation of this expression, except for empty expression. */
  override def toString: String = print
  /* Partial String representation of this Scheme expression. Used to construct `toString` */
  private[scheme] def print: String
}

/** Represents the empty expression. */
case object Empty extends Expression {
  def apply(): Empty.type = Empty
  override def print: String = "()"
  override def printAST: String = print
}

/** Represents a Scheme list expression. */
case class Cons(car: Expression, cdr: Expression) extends Expression {
  override def print: String = cdr match {
    case Empty => "" + car
    case _ => car + " " + cdr.print
  }

  override def toString: String = "(" + print + ")"
  override def printAST: String = "List(" + car.printAST + ", " + cdr.printAST + ")"
  override def evaluate(implicit env: Environment): Expression = car match {
    case Symbol(s) if s == "begin" => cdr.evaluate.simplify.asInstanceOf[Cons].car
    case _ => Cons(car.evaluate, cdr.evaluate)
  }
  override def simplify: Expression = car match {
    case Empty => cdr.simplify
    case _ => Cons(car, cdr.simplify)
  }

  override def preprocess: Expression = car match {
    case Symbol(s) if s == "begin"  => Cons(car, cdr.preprocess)
    case Symbol(s) if s == "define" => Functions.define(cdr)
    case Symbol(s) if s == "lambda" => Functions.lambda(cdr)
    case Symbol(s) if s == "if"     => Functions._if(cdr.preprocess)
    case Symbol(s)                  => Functions.symbol(Symbol(s), cdr)
    case _                          => Cons(car.preprocess, cdr.preprocess)
  }
}
object Cons {
  def apply(car: Expression, cdr: Expression): Cons = new Cons(car, cdr)
}

/** Represents Scheme lambda expressions and built in procedures. */
case class Lambda(params: Expression, f: Expression => Expression) extends Expression {
  override def printAST: String = "Lambda(" + params.printAST + ", " + f + ")"
  override def print: String = "lambda " + params
  override def toString: String = "(" + print + ")"
}
object Lambda {
  def apply(params: Expression, f: Expression => Expression): Expression = new Lambda(params, f)
}

/** Represents Scheme procedure call expressions. */
case class Procedure(op: Symbol, args: Expression) extends Expression {
  override def print: String = op + " " + args.print
  override def toString: String = "(" + print + ")"
  override def printAST: String = "Function(" + op.printAST + ", " + args.printAST + ")"
  override def evaluate(implicit env: Environment): Expression = op.evaluate match {
    case Lambda(params, f) =>
      env.push(Environment.bind(params, args.evaluate))
      val exp = f(params.evaluate)
      env.pop
      exp
  }
}

object Procedure {
  def apply(op: Symbol, args: Expression): Procedure = new Procedure(op, args)
}

/**
 * Represents Scheme symbol expressions.
 * @param symbol Scheme Symbol represented as a String
 */
case class Symbol(symbol: String) extends Expression {
  override def print: String = symbol
  override def printAST: String = "Symbol(" + symbol + ")"
  override def evaluate(implicit env: Environment): Expression = env.get(this)
  override def hashCode(): Int = symbol.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case Symbol(s) => symbol.equals(s)
    case _ => false
  }
}
object Symbol {
  def apply(symbol: String): Symbol = new Symbol(symbol)
}

/**
 * Scheme primitive values are represented as an abstract class and multiple case classes
 */
sealed abstract class Value extends Expression

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
 * Represents Scheme number expressions
 * @param value Double value of Scheme number
 */
case class Number(value: BigDecimal) extends Value {
  def < (expr: Expression) : Expression = expr match {
    case Number(b) => Bool(value < b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def + (expr: Expression) : Expression = expr match {
    case Number(b) => Number(value + b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def - (expr: Expression) : Expression = expr match {
    case Number(b) => Number(value - b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def * (expr: Expression) : Expression = expr match {
    case Number(b) => Number(value * b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  def / (expr: Expression) : Expression = expr match {
    case Number(b) => Number(value / b)
    case _ => throw new EvaluateError(expr + " is not a number")
  }
  override def print: String = value.toString
  override def printAST: String = "Num(" + value + ")"
}
object Number {
  def apply(n: BigDecimal): Number = new Number(n)
  def from(s: String): Number = Number(BigDecimal(s))
}

/**
 * Represents Scheme boolean expressions
 * @param value Boolean value of Scheme boolean
 */
case class Bool(value: Boolean) extends Value {
  override def print: String = if (value) "#t" else "#f"
  override def printAST: String = "Bool(" + value + ")"
}
object Bool {
  def apply(n: Boolean): Bool = new Bool(n)
}