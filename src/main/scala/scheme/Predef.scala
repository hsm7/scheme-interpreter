package scheme

/** Predefined Scheme procedures */
object Predef {

  def empty: Expression => Expression = {
    case Cons(car, _) => car match {
      case Empty => Bool(true)
      case _ => Bool(false)
    }
  }

  def equal: Expression => Expression = {
    case Cons(car, cdr) => Bool(car == Cons.car(cdr))
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
