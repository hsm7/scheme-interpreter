package scheme

/** Predefined Scheme procedures */
object Predef {

  /** Represents Scheme runtime errors */
  class EvaluationError(expr: Expression, msg: String = "Invalid expression: ") extends RuntimeException(msg + expr)

  /* Check whether a list is Empty */
  def empty: Expression => Bool = {
    case Cons(Empty, _) => Bool(true)
    case Cons(_, _) => Bool(false)
    case expr => throw new EvaluationError(expr)
  }

  /* Check whether two lists are equal */
  def equal: Expression => Bool = {
    case Cons(car, cdr) => Bool(car == cdr.car)
    case expr => throw new EvaluationError(expr)
  }

  /* Retrieve car element from a Scheme list */
  def car: Expression => Expression = {
    case Cons(Cons(car, _), _) => car
    case Cons(Empty, _) => Empty
    case expr => throw new EvaluationError(expr)
  }

  /* Retrieve cdr elements from a Scheme list */
  def cdr: Expression => SList = {
    case Cons(Cons(_, cdr), _) => cdr
    case expr => throw new EvaluationError(expr)
  }

  /* Add new element to a Scheme list */
  def cons: Expression => SList = {
    case Cons(car, Cons(Cons(_car, _cdr), _)) => car :: _car :: _cdr
    case Cons(car, Empty) => car :: Empty
    case expr => throw new EvaluationError(expr)
  }

  /* Compare Scheme number expressions */
  def lt: Expression => Bool = {
    case Cons(Number(n), Cons(Number(m), _)) => Bool(n < m)
    case expr => throw new EvaluationError(expr)
  }

  /* Addition of Scheme number expressions. */
  def add: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n + m)
    case expr => throw new EvaluationError(expr)
  }

  /* Subtraction of Scheme number expressions. */
  def subtract: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n - m)
    case expr => throw new EvaluationError(expr)
  }

  /* Multiplication of Scheme number expressions. */
  def multiply: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n * m)
    case expr => throw new EvaluationError(expr)
  }

  /* Division of Scheme number expressions. */
  def divide: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n / m)
    case expr => throw new EvaluationError(expr)
  }

}
