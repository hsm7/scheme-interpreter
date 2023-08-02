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
    val program: String = "((define x 4) (define y -8) (* x (- 5 y)))"
    val ast = Expression.parse(program)
    println(ast.printAST)
    println(ast.preprocess.printAST)
    println(ast.preprocess.evaluate.printAST)
    println(ast.preprocess.evaluate.simplify.printAST)
//    println(Expression.evaluate(ast))
  }
}
