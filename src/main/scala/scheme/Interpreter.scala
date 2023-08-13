package scheme

import scala.annotation.tailrec

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

    def contains(s: Symbol): Boolean = global contains s

    @tailrec
    def update(params: Expression, args: Expression): Expression = {
      params match {
        case Empty => Empty()
        case Cons(car, cdr) => car match {
          case Symbol(s) => args match {
            case Cons(h, t) =>
              put(Symbol(s), h)
              update(cdr, t)
          }
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val program: String = "(define circle (lambda (r) (* 3.14 (* r r))))"
    val define: String = "(define double (lambda (n) (+ n n)))"
    val call: String = "(double (circle 7))"
    Expression.evaluate(Expression.parse(program))
    Expression.evaluate(Expression.parse(define))
    println(Expression.parse(call).printAST)
    println(Expression.parse(call).preprocess.printAST)
    println(Expression.evaluate(Expression.parse(call)).printAST)

  }
}
