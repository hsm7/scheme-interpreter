package scheme

/** Predefined Scheme procedures */
object Predef {

  def empty: Expression => Bool = {
    case Cons(Empty, _) => Bool(true)
    case Cons(_, _) => Bool(false)
    case expr => throw new EvaluateError(expr)
  }

  def equal: Expression => Bool = {
    case Cons(car, cdr) => Bool(car == cdr.car)
    case expr => throw new EvaluateError(expr)
  }

  /* Retrieve car element from a Scheme list */
  def car: Expression => Expression = {
    case Cons(Cons(car, _), _) => car
    case Cons(Empty, _) => Empty
    case expr => throw new EvaluateError(expr)
  }

  /* Retrieve cdr elements from a Scheme list */
  def cdr: Expression => SList = {
    case Cons(Cons(_, cdr), _) => cdr
    case expr => throw new EvaluateError(expr)
  }

  /* Add new element to a Scheme list */
  def cons: Expression => SList = {
    case Cons(car, Cons(Cons(_car, _cdr), _)) => car :: _car :: _cdr
    case Cons(car, Empty) => car :: Empty
    case expr => throw new EvaluateError(expr)
  }

  /* Compare Scheme number expression */
  def lt: Expression => Bool = {
    case Cons(Number(n), Cons(Number(m), _)) => Bool(n < m)
    case expr => throw new EvaluateError(expr)
  }

  /* Addition of Scheme number expression */
  def add: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n + m)
    case expr => throw new EvaluateError(expr)
  }

  /* Subtraction of Scheme number expression. */
  def subtract: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n - m)
    case expr => throw new EvaluateError(expr)
  }

  /* Multiplication of Scheme number expression. */
  def multiply: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n * m)
    case expr => throw new EvaluateError(expr)
  }

  /* Division of Scheme integer and number expression. */
  def divide: Expression => Number = {
    case Cons(Number(n), Cons(Number(m), _)) => Number(n / m)
    case expr => throw new EvaluateError(expr)
  }

}
