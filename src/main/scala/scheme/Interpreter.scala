package scheme

import Environment.global

object Interpreter {

  implicit class EvaluateString(val value: String) extends AnyVal {
    def eval (implicit env: Environment): Expression = Expression.evaluate(Expression.parse(value))(env)
  }

  def main(args: Array[String]): Unit = {
    val program: String = "(begin (define circle (lambda (r) (* pi (* r r)))) (define double (lambda (n) (+ n n))) (double (circle 6.5)))"
    val factorial: String = "(begin (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1)))))) (fact 10))"
    val fibonacci: String = "(begin (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 2)) (fib (- n 1)))))) (fib 10))"
    println("=> " + program)
    println(program.eval)
    println("=> " + fibonacci)
    println(fibonacci.eval)
    println("=> " + factorial)
    println(factorial.eval)
  }
}
