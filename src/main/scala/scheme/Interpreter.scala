package scheme

object Interpreter {

  class SyntaxError(msg: String = "Lots of Irritating Silly Parentheses!") extends RuntimeException(msg)
  class EvaluateError(msg: String = "Invalid expression") extends RuntimeException(msg)

  def main(args: Array[String]): Unit = {
    val program: String = "(car (cdr (cons (+ 1 6.1 (+ 5.4 1.5) (+ 2 3)) (20 21.0))))"
    val ast = Expression.parse(program)
    println(ast.printAST)
    println(ast)
    println(Expression.evaluate(ast))
  }
}
