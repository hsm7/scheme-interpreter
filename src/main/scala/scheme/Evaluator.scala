package scheme

trait Evaluator[T <: Expression] {
    /**
     * Evaluate Scheme expression.
     *
     * @param expr scheme expression to evaluate
     * @param env  environment in which `expr` will be evaluated
     * @return result of evaluating `expr`
     */
    def evaluate(expr: T)(implicit env: Environment): Expression
}

object Evaluator {
    def apply[T <: Expression : Evaluator]: Evaluator[T] = implicitly[Evaluator[T]]

    implicit object ExpressionEvaluator extends Evaluator[Expression] {
        override def evaluate(expr: Expression)(implicit env: Environment): Expression = {
            expr match {
                case p: Procedure => ProcedureEvaluator.evaluate(p)
                case s: Symbol => SymbolEvaluator.evaluate(s)
                case c: Cons => ConsEvaluator.evaluate(c)
                case v: Value => ValueEvaluator.evaluate(v)
                case e: Empty.type => EmptyEvaluator.evaluate(e)
            }
        }
    }

    implicit object ConsEvaluator extends Evaluator[Cons] {
        val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]

        override def evaluate(list: Cons)(implicit env: Environment): Expression = list.car match {
            case Symbol(s) if s == "begin" => begin(list.cdr)
            case Symbol(s) if s == "quote" => list.cdr.car
            case Symbol(s) if s == "define" => define(list.cdr)
            case Symbol(s) if s == "set!" => set(list.cdr)
            case Symbol(s) if s == "lambda" => procedure(Symbol(s), list.cdr)
            case Symbol(s) if s == "if" => _if(list.cdr)
            case s: Symbol => evaluator.evaluate(s) match {
                case Procedure(op, _, params, f) => list.cdr match {
                    case _: Cons => evaluator.evaluate(Procedure(op, list.cdr, params, f))
                    case Empty => Procedure(op, list.cdr, params, f) :: Empty
                }
                case expr => expr :: evaluator.evaluate(list.cdr).asInstanceOf[SList]
            }
            case _ => simplify(evaluator.evaluate(list.car) :: evaluator.evaluate(list.cdr).asInstanceOf[SList])
        }

        /* Simplify an expression. Remove Empty expressions after evaluating define expressions */
        private def simplify(expr: SList): SList = expr match {
            case Cons(Empty, cdr) => simplify(cdr)
            case Cons(car, cdr) => car :: simplify(cdr)
            case Empty => Empty
        }

        /** Construct Scheme procedure expressions */
        private def procedure(op: Symbol, exp: SList): Expression = {
            exp match {
                case Cons(car: Cons, cdr) => Procedure(op, car, closure => _ => evaluator.evaluate(cdr.car)(closure))
            }
        }

        /** Evaluate Scheme begin expressions */
        private def begin(params: SList)(implicit env: Environment): Expression = evaluator.evaluate(params) match {
            case Cons(car, _) => car
            case exp: Expression => exp
        }

        /** Evaluate Scheme if expressions */
        private def _if(exp: SList)(implicit env: Environment): Expression = exp match {
            case Cons(car, cdr) => evaluator.evaluate(car) match {
                case Bool(s) if s => evaluator.evaluate(cdr.car)
                case Bool(s) if !s => evaluator.evaluate(cdr.cdr.car)
            }
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

        override def evaluate(proc: Procedure)(implicit env: Environment): Expression = {
            val args = evaluator.evaluate(proc.args).asInstanceOf[SList]
            val scope = Environment(Environment.from(proc.params, args), env)
            proc.f(scope)(evaluator.evaluate(proc.params)(scope))
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

}