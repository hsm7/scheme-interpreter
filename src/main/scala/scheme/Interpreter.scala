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
    val define: String = "(define square (lambda (n) (* n n)))"
    val call: String = "(square 5)"
    val ast = Expression.parse(define)
    Expression.evaluate(ast)
    Environment.put(Symbol("n"), Number(11))

    println(Expression.parse(call).printAST)
    println(Expression.parse(call).preprocess.printAST)
    println(Expression.evaluate(Expression.parse(call)))

  }
}
