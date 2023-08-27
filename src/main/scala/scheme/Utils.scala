package scheme

/** Utils to construct and evaluate Scheme expressions */
object Utils {

  /** Construct Scheme lambda expressions */
  def lambda: Expression => Expression = {
    case Cons(car, cdr) => Lambda(car, _ => Cons.car(cdr).evaluate)
  }

  /** Construct Scheme procedure expressions */
  def symbol(symbol: Symbol, expr: Expression)(implicit env: Environment): Expression = symbol.evaluate match {
    case Lambda(_, _) => Procedure(symbol, expr).evaluate
    case _ => Cons(symbol.evaluate, expr.evaluate)
  }

  /** Evaluate Scheme if expressions */
  def _if: Expression => Expression = {
    case Cons(car, cdr) => car.evaluate match {
      case Bool(s) if s => Cons.car(cdr)
      case Bool(s) if !s => Cons.car(Cons.cdr(cdr))
    }
  }

  /** Map a Scheme symbol to an expression in the given environment */
  def define(exp: Expression)(implicit env: Environment): Expression = exp match {
    case Cons(car, cdr) => car match {
      case Symbol(s) =>
        env.put(Symbol(s), Cons.car(cdr).evaluate)
        Empty()
    }
  }

  /** Scheme set! expression */
  def set(exp: Expression)(implicit env: Environment): Expression = exp match {
    case Cons(car, cdr) => car match {
      case Symbol(s) =>
        env.update(Symbol(s), Cons.car(cdr))
        Empty()
    }
  }
}