package scheme

import scheme.Interpreter.SyntaxError

import scala.util.parsing.combinator.RegexParsers

/* Scheme parser using RegexParsers from scala parser combinator */
object Parser extends RegexParsers {

  // Scheme boolean is either `#t` or `#f`
  def bool: Parser[Bool] = ("#t" | "#f") ^^ { case "#t" => Value(true); case "#f" => Value(false) }

  // Scheme integer is an optional `-` followed by one or more digits
  def int: Parser[Integer] = "-?\\d+".r ^^ (s => Value(s.toInt))

  // Scheme number is an optional `-` followed by one or more digits, followed by `.` and zero or more digits
  def num: Parser[Number] = "-?\\d+\\.\\d*".r ^^ (s => Value(s.toDouble))

  // Scheme string can have any character except `"` wrapped in ""
  def str: Parser[Str] = "\"" ~> "[^\"]*".r <~ "\"" ^^ Str

  // Scheme primitive values can be any of the these patterns
  def value: Parser[Value] = bool | num | int | str

  // Scheme identifiers allows alphanumeric chars, some symbols, and can't start with a digit
  def symbol : Parser[Symbol] = "[a-zA-Z=*+-/<>!\\?][a-zA-Z0-9=*+-/<>!\\?]*".r ^^ (s => Symbol(s))

  // Scheme list is a series of expressions wrapped in ()
  def scheme : Parser[Expression] = "(" ~> rep(expr) <~ ")" ^^ list

  // Scheme Function is a symbol followed by a series of expressions both are wrapped in ()
  def func : Parser[Func] = "(" ~> symbol ~ rep(expr) <~ ")" ^^ func

  // A Scheme expression can be any of the previous patterns
  def expr : Parser[Expression] = value | symbol | func | scheme

  /**
   * Recursively parse Scheme expression from a list of scheme expressions.
   * @param expressions list of expressions to parse
   * @return expression AST for input Scheme expressions
   */
  def list(expressions: List[Expression]): Expression = expressions match {
    case List() => Empty
    case car :: cdr => Cons(car, list(cdr))
  }

  /**
   * Recursively parse Scheme function expression from input Symbol and list of
   * scheme expressions.
   * @param s pair of a Scheme Symbol and a list of Scheme Expressions
   * @return Function expression AST for input Scheme Symbol and expressions
   */
  def func(s: Symbol ~ List[Expression]): Func = s._2.head match {
    case _ @ (Integer(_) | Number(_)) => s._1.symbol match {
      case "+" => Func(s._1, list(s._2), Functions.fold(Functions.add, Value(0)))
      case "-" => Func(s._1, list(s._2), Functions.fold(Functions.subtract, Value(0)))
      case "*" => Func(s._1, list(s._2), Functions.fold(Functions.multiply, Value(1)))
      case "/" => Func(s._1, list(s._2), Functions.fold(Functions.divide, Value(1)))
    }
    case _ => throw new UnsupportedOperationException
  }

  /**
   * Parse Scheme expression from a string input.
   * @param program input to parse
   * @return expression AST for input Scheme `program`
   */
  def parse(program: String): Expression =
    parse(expr, program) match {
      case Success(matched,_) => matched
      case Failure(msg,_) => throw new SyntaxError(msg)
      case Error(msg,_) => throw new SyntaxError(msg)
    }

  def main(args: Array[String]): Unit = {
    val program = "(1.2 (* 3.3 (+ 9 0 8)) 9)"
    parse(expr, program) match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}