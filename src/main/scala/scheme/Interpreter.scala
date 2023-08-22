package scheme

import Environment.global

object Interpreter {

  def main(args: Array[String]): Unit = {
    val program: String = "(define circle (lambda (r) (* pi (* r r))))"
    val define: String = "(define double (lambda (n) (+ n n)))"
    val factorial: String = "(begin (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1)))))) (fact 10))"
    val fibonacci: String = "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))"
    val call: String = "(circle (double x))"
    val fib: String = "(fib 10)"
    Expression.evaluate(Expression.parse(program))
    Expression.evaluate(Expression.parse(define))
    Expression.evaluate(Expression.parse(fibonacci))
    val double = Expression.parse(call)
    val fib10 = Expression.parse(fib)
    val body = Expression.parse("(+ 1 x)")
    val lambda = Lambda(Cons(Symbol("x"), Empty), _ => body.preprocess.evaluate)
    global.put(Symbol("plusOne"), lambda)
    val fun = Procedure(Symbol("plusOne"), Cons(Number(41), Empty))
    global.put(Symbol("x"),   Integer(7))
    println(fun + " => " + Expression.evaluate(fun))
    println(double + " => " + Expression.evaluate(double))
    println(fib10 + " => " + Expression.evaluate(fib10))
    println(factorial + " => " + Expression.evaluate(Expression.parse(factorial)))
    println(global.size)
  }
}
