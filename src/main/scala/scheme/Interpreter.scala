package scheme

import scala.annotation.tailrec

object Interpreter {

  class SyntaxError(msg: String = "Lots of Irritating Silly Parentheses!") extends RuntimeException(msg)
  class EvaluateError(msg: String = "Invalid expression") extends RuntimeException(msg)
  object Environment {
    val stack: collection.mutable.Stack[collection.mutable.Map[Symbol, Expression]] = collection.mutable.Stack.empty

    create()
    val params: Cons = Cons(Symbol("x"), Cons(Symbol("y"), Empty))
    Environment.put(Symbol("+"),    Lambda(params, Functions.fold(Functions.add)))
    Environment.put(Symbol("-"),    Lambda(params, Functions.fold(Functions.subtract)))
    Environment.put(Symbol("*"),    Lambda(params, Functions.fold(Functions.multiply)))
    Environment.put(Symbol("/"),    Lambda(params, Functions.fold(Functions.divide)))
    Environment.put(Symbol("car"),  Lambda(Cons(Symbol("list"), Empty), Functions.car))
    Environment.put(Symbol("cdr"),  Lambda(Cons(Symbol("list"), Empty), Functions.cdr))
    Environment.put(Symbol("cons"), Lambda(params, Functions.cons))
    Environment.put(Symbol("pi"),   Number(3.14))

    def get(s: Symbol): Expression = {
      stack.find(_.contains(s)).getOrElse(throw new NoSuchElementException(s"$s is not defined"))(s)
    }

    def contains(s: Symbol): Boolean = stack.exists(_.contains(s))

    def put(s: Symbol, expr: Expression): Expression = {
      stack.top.put(s, expr)
      Empty()
    }

    @tailrec
    def update(params: Expression, args: Expression): Expression = params match {
      case Empty => Empty()
      case Cons(car, cdr) => car match {
        case Symbol(s) => args match {
          case Cons(h, t) =>
            put(Symbol(s), h)
            update(cdr, t)
        }
      }
    }

    def create(): Unit = stack.push(collection.mutable.Map.empty)
    def destroy(): Unit = stack.pop()
  }

  def main(args: Array[String]): Unit = {
    val program: String = "(define circle (lambda (r) (* pi (* r r))))"
    val define: String = "(define double (lambda (n) (+ n n)))"
    val call: String = "(circle (double 7))"
    Expression.evaluate(Expression.parse(program))
    Expression.evaluate(Expression.parse(define))
    val double = Expression.parse(call)
    val body = Expression.parse("(+ 1 n)")
    val lambda = Lambda(Cons(Symbol("n"), Empty), _ => body.preprocess.evaluate)
    Environment.put(Symbol("plusOne"), lambda)
    val fun = Procedure(Symbol("plusOne"), Cons(Number(41), Empty))
    println(fun + " => " + Expression.evaluate(fun))
    println(double + " => " + Expression.evaluate(double))
  }
}
