package scheme

import scala.annotation.tailrec

trait Evaluator[T <: Expression] {
  /**
   * Evaluate Scheme expression.
   * @param expr scheme expression to evaluate
   * @param env environment in which `expr` will be evaluated
   * @return result of evaluating `expr`
   */
  def evaluate(expr: T)(env: Environment): Expression
}

object Evaluator {
  def apply[T <: Expression : Evaluator]: Evaluator[T] = implicitly[Evaluator[T]]

  implicit object ExpressionEvaluator extends Evaluator[Expression] {
    override def evaluate(expr: Expression)(env: Environment): Expression = {
      expr match {
        case v: Value      => ValueEvaluator.evaluate(v)(env)
        case s: Symbol     => SymbolEvaluator.evaluate(s)(env)
        case p: Procedure  => ProcedureEvaluator.evaluate(p)(env)
        case c: Cons       => ConsEvaluator.evaluate(c)(env)
        case e: Empty.type => EmptyEvaluator.evaluate(e)(env)
      }
    }
  }

  implicit object ConsEvaluator extends Evaluator[Cons] {
    val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]

    override def evaluate(list: Cons)(env: Environment): Expression = list.car match {
      case Symbol(s) if s == "begin"  => begin(list.cdr)(env)
      case Symbol(s) if s == "quote"  => list.cdr.car
      case Symbol(s) if s == "define" => define(list.cdr)(env)
      case Symbol(s) if s == "set!"   => set(list.cdr)(env)
      case Symbol(s) if s == "lambda" => lambda(list.cdr)
      case Symbol(s) if s == "if"     => _if(list.cdr)(env)
      case s: Symbol => symbol(s, list.cdr)(env)
      case _  => simplify(evaluator.evaluate(list.car)(env) :: evaluator.evaluate(list.cdr)(env).asInstanceOf[SList])
    }

    /* Simplify an expression. Remove Empty expressions after evaluating define expressions */
    private def simplify(expr: SList): SList = expr match {
      case Cons(Empty, cdr) => simplify(cdr)
      case Cons(car, cdr) => car :: simplify(cdr)
      case Empty => Empty
    }

    /** Construct Scheme lambda expressions */
    private def lambda(exp: SList): Expression = exp match {
      case Cons(car: Cons, cdr) =>
        Lambda(car, closure => _ => evaluator.evaluate(cdr.car)(closure))
    }

    /** Evaluate Scheme begin expressions */
    private def begin(params: SList)(env: Environment): Expression = evaluator.evaluate(params)(env) match {
      case Cons(car, _) => car
      case exp: Expression => exp
    }

    /** Construct Scheme procedure expressions */
    private def symbol(symbol: Symbol, expr: SList)(env: Environment): Expression = {
      val e = evaluator.evaluate(symbol)(env)
      e match {
        case l: Lambda => expr match {
          case Empty => l
          case _ => evaluator.evaluate(Procedure(symbol, expr))(env)
        }
        case _ => e  :: evaluator.evaluate(expr)(env).asInstanceOf[SList]
      }
    }

    /** Evaluate Scheme if expressions */
    private def _if(exp: SList)(env: Environment): Expression = exp match {
        case Cons(car, cdr) => evaluator.evaluate(car)(env) match {
          case Bool(s) if s => evaluator.evaluate(cdr.car)(env)
          case Bool(s) if !s => evaluator.evaluate(cdr.cdr.car)(env)
        }
      }

    /** Map a Scheme symbol to an expression in the given environment */
    private def define(exp: SList)(env: Environment): Expression = exp match {
      case Cons(s: Symbol, cdr) =>
        env.put(s, evaluator.evaluate(cdr.car)(env))
        Empty
    }

    /** Scheme set! expression */
    private def set(exp: SList)(env: Environment): Expression = exp match {
      case Cons(s: Symbol, cdr) =>
        env.update(s, evaluator.evaluate(cdr.car)(env))
        Empty
    }
  }

  implicit object ProcedureEvaluator extends Evaluator[Procedure] {
    val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]
    override def evaluate(proc: Procedure)(env: Environment): Expression =
      evaluator.evaluate(proc.op)(env) match {
      case Lambda(params, f) =>
        val args = evaluator.evaluate(proc.args)(env).asInstanceOf[SList]
        val scope = Environment(Environment.from(params, args), env)
        val p = evaluator.evaluate(params)(scope)
        f(scope)(p)
      }
  }

  implicit object SymbolEvaluator extends Evaluator[Symbol] {
    override def evaluate(symbol: Symbol)(env: Environment): Expression = env.get(symbol)
  }

  implicit object EmptyEvaluator extends Evaluator[Empty.type] {
    override def evaluate(empty: Empty.type)(env: Environment): Expression = empty
  }

  implicit object ValueEvaluator extends Evaluator[Value] {
    override def evaluate(value: Value)(env: Environment): Expression = value
  }

  def main(args: Array[String]): Unit = {
    val factorial: String = "(begin (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1)))))) (fact 10))"
    val count: String = "(begin (define first car) (define rest cdr) (define count (lambda (item L) (if (empty? L) 0 (+ (if (equal? item (first L)) 1 0) (count item (rest L)))))))"
    val more: String = "(count (quote the) (quote (the more the merrier the bigger the better)))"

    import Environment.global

    println(Evaluator[Value].evaluate(Number(7))(global))
    println(Evaluator[Expression].evaluate(Str("String"))(global))
    println(Evaluator[Value].evaluate(Bool(true))(global))
    println(Evaluator[Expression].evaluate(Parser.parse(factorial))(global))
    Evaluator[Expression].evaluate(Parser.parse(count))(global)
    println(Evaluator[Expression].evaluate(Parser.parse(more))(global))
  }
}