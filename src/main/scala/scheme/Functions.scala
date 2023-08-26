package scheme

/* Utils to construct and evaluate Scheme expressions */
object Functions {

  /** Construct Schem lambda expressions */
  def lambda: Expression => Expression = {
    case Cons(car, cdr) => Lambda(car, _ => Cons.car(cdr).preprocess.evaluate)
  }

  /** Construct Scheme procedure expressions */
  def symbol(symbol: Symbol, expr: Expression)(implicit env: Environment): Expression = env.get(symbol) match {
    case Lambda(_, _) => Procedure(symbol, expr.preprocess)
    case _ => Cons(symbol, expr.preprocess)
  }

  /** Evaluate Scheme if expression */
  def _if: Expression => Expression = {
    case Cons(car, cdr) => car.evaluate match {
      case Bool(s) if s => Cons.car(cdr)
      case Bool(s) if !s => Cons.car(Cons.cdr(cdr))
    }
  }

  /** Map a Scheme symbol to an expression in the global environment */
  def define(exp: Expression)(implicit env: Environment): Expression = exp match {
    case Cons(car, cdr) => car match {
      case Symbol(s) =>
        env.put(Symbol(s), Cons.car(cdr).preprocess)
        Empty()
    }
  }

  /* Retrieve car element from a Scheme list */
  def car: Expression => Expression = {
    case Cons(car, _) => Cons.car(car)
    case exp => throw new EvaluateError(exp + " is not a list")
  }

  /* Retrieve cdr elements from a Scheme list */
  def cdr: Expression => Expression = {
    case Cons(car, _) => Cons.cdr(car)
    case exp => throw new EvaluateError(exp + " is not a list")
  }

  /* Add new element to a Scheme list */
  def cons: Expression => Expression = {
    case Cons(car, cdr) => Cons(car, Cons.car(cdr))
    case exp => throw new EvaluateError(exp + " is not a list")
  }

  /* Compare Scheme number expression */
  def lt: Expression => Expression = {
    case Cons(car, cdr) => car match {
      case Number(a) => Number(a) < Cons.car(cdr)
      case _ => throw new EvaluateError(car + " is not a number")
    }
  }

  /* Addition of Scheme number expression */
  def add: Expression => Expression = {
    case Cons(car, cdr) => car match {
      case Number(a) => Number(a) + Cons.car(cdr)
      case _ => throw new EvaluateError(car + " is not a number")
    }
  }

  /* Subtraction of Scheme number expression. */
  def subtract: Expression => Expression = {
    case Cons(car, cdr) => car match {
      case Number(a) => Number(a) - Cons.car(cdr)
      case _ => throw new EvaluateError(car + " is not a number")
    }
  }

  /* Multiplication of Scheme number expression. */
  def multiply: Expression => Expression = {
    case Cons(car, cdr) => car match {
      case Number(a) => Number(a) * Cons.car(cdr)
      case _ => throw new EvaluateError(car + " is not a number")
    }
  }

  /* Division of Scheme integer and number expression. */
  def divide: Expression => Expression = {
    case Cons(car, cdr) => car match {
      case Number(a) => Number(a) / Cons.car(cdr)
      case _ => throw new EvaluateError(car + " is not a number")
    }
  }
}