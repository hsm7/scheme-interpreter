package scheme


/* Expression ADT represents Scheme expressions abstract syntax tree */
sealed trait Expression {
  // Datatype definition:
  //    Expression  = SList | Value | Symbol | Procedure
  //    SList       = Empty | Cons(car: Expression, cdr: SList)
  //    Value       = Number(value: BigDecimal) | Bool(value: Boolean) | Str(value: String)
  //                | Lambda(params: SList, f: Expression => Expression)
  //    Symbol      = Symbol(s: String)
  //    Procedure   = Procedure(op: Symbol, args: SList)

  /* String representation of abstract syntax tree for this Scheme expression. */
  def printAST: String
  /* Parsable and valid Scheme String representation of this expression. */
  override def toString: String = print
  /* Partial String representation of this Scheme expression. Used to construct `toString` */
  private[scheme] def print: String
}

/** Represents a Scheme list expression. */
abstract sealed class SList extends Expression {
  def car: Expression
  def cdr: SList
}
object SList {
  implicit class SListEnrichment(val list: SList) extends AnyVal {
    def :: (expr: Expression): SList = Cons(expr, list)
  }
}

/** Represents Scheme empty list expression. */
case object Empty extends SList {
  override def car: Expression = this
  override def cdr: SList = this
  override def print: String = "()"
  override def printAST: String = print

}

/** Represents Scheme non-empty list expressions. */
case class Cons(car: Expression, cdr: SList) extends SList {
  override def toString: String = "(" + print + ")"
  override def printAST: String = "List(" + car.printAST + ", " + cdr.printAST + ")"
  override def print: String = cdr match {
    case Empty => "" + car
    case _ => car + " " + cdr.print
  }
}

/** Represents Scheme procedure call expressions. */
case class Procedure(op: Expression, args: SList) extends Expression {
  override def print: String = op + " " + args.print
  override def toString: String = "(" + print + ")"
  override def printAST: String = "Procedure(" + op.printAST + ", " + args.printAST + ")"
}

/** Represents Scheme symbol expressions */
case class Symbol(symbol: String) extends Expression {
  override def print: String = symbol
  override def printAST: String = "Symbol(" + symbol + ")"
  override def hashCode(): Int = symbol.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case Symbol(s) => symbol.equals(s)
    case _ => false
  }
}

/** Scheme primitive values are represented as an abstract class and multiple case classes */
sealed abstract class Value extends Expression

/** Represents Scheme lambda expressions and built in procedures. */
case class Lambda(params: SList, f: Environment => Expression => Expression) extends Value {
  override def printAST: String = "Lambda(" + params.printAST + ", " + f + ")"
  override def print: String = "lambda " + params
  override def toString: String = "(" + print + ")"
}

/**
 * Represents Scheme string expressions */
case class Str(value: String) extends Value {
  override def print: String = value
  override def printAST: String = "Str(" + value + ")"
}

/** Represents Scheme number expressions */
case class Number(value: BigDecimal) extends Value {
  override def print: String = value.toString
  override def printAST: String = "Number(" + value + ")"
}
object Number {
  def from(s: String): Number = Number(BigDecimal(s))
}

/** Represents Scheme boolean expressions */
case class Bool(value: Boolean) extends Value {
  override def print: String = if (value) "#t" else "#f"
  override def printAST: String = "Bool(" + value + ")"
}