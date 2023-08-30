package scheme

trait Evaluator[T <: Expression] {
  /**
   * Evaluate Scheme expression.
   * @param expr scheme expression to evaluate
   * @param env environment in which `expr` will be evaluated
   * @return result of evaluating `expr`
   */
  def evaluate(expr: T)(implicit env: Environment): Expression
}

object Evaluator {
  def apply[T <: Expression : Evaluator]: Evaluator[T] = implicitly[Evaluator[T]]

  implicit object ExpressionEvaluator extends Evaluator[Expression] {
    override def evaluate(expr: Expression)(implicit env: Environment): Expression =
      expr match {
        case v: Value      => ValueEvaluator.evaluate(v)
        case s: Symbol     => SymbolEvaluator.evaluate(s)
        case p: Procedure  => ProcedureEvaluator.evaluate(p)
        case c: Cons       => ConsEvaluator.evaluate(c)
        case e: Empty.type => EmptyEvaluator.evaluate(e)
      }
  }

  implicit object ConsEvaluator extends Evaluator[Cons] {
    val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]

    override def evaluate(list: Cons)(implicit env: Environment): Expression = list.car match {
      case Symbol(s) if s == "begin"  => begin(list.cdr)
      case Symbol(s) if s == "quote"  => list.cdr.car
      case Symbol(s) if s == "define" => define(list.cdr)
      case Symbol(s) if s == "set!"   => set(list.cdr)
      case Symbol(s) if s == "lambda" => lambda(list.cdr)
      case Symbol(s) if s == "if"     => _if(list.cdr)
      case s: Symbol => symbol(s, list.cdr)
      case _         => simplify(evaluator.evaluate(list.car) :: evaluator.evaluate(list.cdr).asInstanceOf[SList])
    }

    /* Simplify a Scheme expression. Removes Empty expressions after evaluating
   * define expressions */
    private def simplify(expr: SList): SList = expr match {
      case Cons(Empty, cdr) => simplify(cdr)
      case Cons(car, cdr) => car :: simplify(cdr)
      case Empty => Empty
    }

    /** Construct Scheme lambda expressions */
    private def lambda: SList => Expression = {
      case Cons(car: Cons, cdr) => Lambda(car, _ => evaluator.evaluate(cdr.car))
    }

    /** Evaluate Scheme begin expressions */
    private def begin(params: SList): Expression = evaluator.evaluate(params) match {
      case Cons(car, _) => car
      case exp: Expression => exp
    }

    /** Construct Scheme procedure expressions */
    private def symbol(symbol: Symbol, expr: SList)(implicit env: Environment): Expression =
      evaluator.evaluate(symbol) match {
      case _: Lambda => evaluator.evaluate(Procedure(symbol, expr))
      case _ => evaluator.evaluate(symbol) :: evaluator.evaluate(expr).asInstanceOf[SList]
    }

    /** Evaluate Scheme if expressions */
    private def _if: SList => Expression = {
      case Cons(car, cdr) => evaluator.evaluate(car) match {
        case Bool(s) if s => evaluator.evaluate(cdr.car)
        case Bool(s) if !s => evaluator.evaluate(cdr.cdr.car)
      }
      case expr => throw new EvaluateError(expr)
    }

    /** Map a Scheme symbol to an expression in the given environment */
    private def define(exp: SList)(implicit env: Environment): Expression = exp match {
      case Cons(s: Symbol, cdr) =>
        env.put(s, evaluator.evaluate(cdr.car))
        Empty
    }

    /** Scheme set! expression */
    private def set(exp: SList)(implicit env: Environment): Expression = exp match {
      case Cons(s: Symbol, cdr) =>
        env.update(s, evaluator.evaluate(cdr.car))
        Empty
    }
  }

  implicit object ProcedureEvaluator extends Evaluator[Procedure] {
    val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]
    override def evaluate(proc: Procedure)(implicit env: Environment): Expression =
      evaluator.evaluate(proc.op) match {
      case Lambda(params, f) =>
        env.push(Environment.from(params, evaluator.evaluate(proc.args).asInstanceOf[SList]))
        val exp = f(evaluator.evaluate(params))
        env.pop
        exp
    }
  }

  implicit object SymbolEvaluator extends Evaluator[Symbol] {
    override def evaluate(symbol: Symbol)(implicit env: Environment): Expression = env.get(symbol)
  }

  implicit object EmptyEvaluator extends Evaluator[Empty.type] {
    override def evaluate(empty: Empty.type)(implicit env: Environment): Expression = empty
  }

  implicit object ValueEvaluator extends Evaluator[Value] {
    override def evaluate(value: Value)(implicit env: Environment): Expression = value
  }

  def main(args: Array[String]): Unit = {
    val factorial: String = "(begin (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1)))))) (fact 10))"
    val count: String = "(begin (define first car) (define rest cdr) (define count (lambda (item L) (if (empty? L) 0 (+ (if (equal? item (first L)) 1 0) (count item (rest L)))))))"
    val more: String = "(count (quote the) (quote (the more the merrier the bigger the better)))"
    println(Evaluator[Value].evaluate(Number(7)))
    println(Evaluator[Expression].evaluate(Str("String")))
    println(Evaluator[Value].evaluate(Bool(true)))
    println(Evaluator[Expression].evaluate(Parser.parse(factorial)))
    println(Evaluator[Expression].evaluate(Parser.parse(count)))
    println(Evaluator[Expression].evaluate(Parser.parse(more)))
    println(Evaluator[Expression].evaluate(Parser.parse("(define head car)")))
  }
}