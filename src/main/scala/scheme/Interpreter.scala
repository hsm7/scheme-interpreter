package scheme

import Environment.global

object Interpreter {

  implicit class ParseString(val str: String) extends AnyVal {
    def parse: Expression = Parser.parse(str)
  }

  implicit class EvaluateExpression(val expr: Expression) extends AnyVal {
    def eval (implicit env: Environment): Expression = Evaluator[Expression].evaluate(expr)(env)
  }

  def main(args: Array[String]): Unit = {
    val program: String = "(begin (define circle (lambda (r) (* pi (* r r)))) (define double (lambda (n) (+ n n))) (double (circle 6.5)))"
    val factorial: String = "(begin (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1)))))) (fact 10))"
    val fibonacci: String = "(begin (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 2)) (fib (- n 1)))))) (fib 10))"
    val count: String = "(begin (define first car) (define rest cdr) (define count (lambda (item L) (if (empty? L) 0 (+ (if (equal? item (first L)) 1 0) (count item (rest L)))))))"
    val more: String = "(count (quote the) (quote (the more the merrier the bigger the better)))"
    val twice: String = "(define twice (lambda (x) (* 2 x)))"
    val repeat: String = "(twice 5)"
    val hof: String = "(define repeat (lambda (f) (lambda (x) (f (f x)))))"
    val repeatTwice = "((repeat twice) 10)"
    count.parse.eval
    println("=> " + program)
    println(program.parse.eval)
    println("=> " + fibonacci)
    println(fibonacci.parse.eval)
    println("=> " + factorial)
    println(factorial.parse.eval)
    println("=> " + more)
    println(more.parse.eval)
    twice.parse.eval
    hof.parse.eval
    println(repeat.parse.eval)
//    println(repeatTwice.parse.eval.eval)
  }
}
