package scheme

import scala.util.parsing.combinator.RegexParsers

/* Scheme parser using RegexParsers from scala parser combinator */
object Parser extends RegexParsers {

  class SyntaxError(msg: String = "Lots of Irritating Silly Parentheses!") extends RuntimeException(msg)

  // Scheme boolean is either `#t` or `#f`
  def bool: Parser[Bool] = ("#t" | "#f") ^^ {
    case "#t" => Bool(true)
    case "#f" => Bool(false)
  }

  // Scheme integer is an optional `-` followed by one or more digits
  def int: Parser[Integer] = "-?\\d+".r ^^ Integer.from

  // Scheme number is an optional `-` followed by one or more digits, followed by `.` and zero or more digits
  def num: Parser[Number] = "-?\\d+\\.\\d*".r ^^ Number.from

  // Scheme string can have any character except `"` wrapped in ""
  def str: Parser[Str] = "\"" ~> "[^\"]*".r <~ "\"" ^^ Str.apply

  // Scheme primitive values can be any of the these patterns
  def value: Parser[Value] = bool | num | int | str

  // Scheme identifiers allows alphanumeric chars, some symbols, and can't start with a digit
  def symbol : Parser[Symbol] = "[a-zA-Z=*+-/<>!\\?][a-zA-Z0-9=*+-/<>!\\?]*".r ^^ Symbol.apply

  // Scheme list is a series of expressions wrapped in ()
  def list : Parser[Expression] = "(" ~> rep(expr) <~ ")" ^^ list

  // A Scheme expression can be any of the previous patterns
  def expr : Parser[Expression] = value | symbol | list

  /**
   * Recursively parse Scheme expression from a list of scheme expressions.
   * @param expressions list of expressions to parse
   * @return expression AST for input Scheme expressions
   */
  def list(expressions: List[Expression]): Expression = expressions match {
    case Nil     => Empty
    case car :: cdr => Cons(car, list(cdr))
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
    val program = "(car (7))"
    parse(expr, program) match {
      case Success(matched,_) => println(matched.printAST)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}