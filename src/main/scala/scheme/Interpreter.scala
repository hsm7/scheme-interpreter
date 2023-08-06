package scheme

object Interpreter {

  class SyntaxError(msg: String = "Lots of Irritating Silly Parentheses!") extends RuntimeException(msg)
  class EvaluateError(msg: String = "Invalid expression") extends RuntimeException(msg)
  object Environment {
    var global: Map[Symbol, Expression] = Map()

    def put(s: Symbol, expr: Expression): Expression = {
      global = global updated(s, expr)
      Empty()
    }
    def get(s: Symbol): Expression = global(s)
  }

  def main(args: Array[String]): Unit = {
    val program: String = "(define circle-area (lambda (r) (* 3.14 (* r r))))"
    val call: String = "(lambda (r) (* 3.14 (* r r)))"
    val ast = Expression.parse(call)
    println(ast.printAST)
    println(ast.preprocess.printAST)
    Environment.put(Symbol("r"), Number(11))
    println(ast.preprocess.evaluate)
  }
}
