package scheme

import scheme.Interpreter.EvaluateError

/* Functional utils to construct Scheme function expressions */
object Functions {
  // An associative binary function of Scheme expressions
  type BiFunc = (Expression, Expression) => Expression

  /* Addition of Scheme integer and number expression */
  def add: BiFunc = (n, m) => n match {
    case Integer(a) => Value(a) + m
    case Number(a)  => Value(a) + m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Subtraction of Scheme integer and number expression. */
  def subtract: BiFunc = (n, m) => n match {
    case Integer(a) => Value(a) - m
    case Number(a)  => Value(a) - m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Multiplication of Scheme integer and number expression. */
  def multiply: BiFunc = (n, m) => n match {
    case Integer(a) => Value(a) * m
    case Number(a)  => Value(a) * m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Division of Scheme integer and number expression. */
  def divide: BiFunc = (n, m) => n match {
    case Integer(a) => Value(a) / m
    case Number(a)  => Value(a) / m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /**
   * Folds the curried expression using a binary operator, starting from an identity
   * expression.
   * @param f binary function to apply.
   * @param identity expression to start folding with.
   * @return resulting expression obtained by folding elements of the curried expression
   *         with `f` function, starting from `identity`. Or return an expression mapping
   *         function (Expression => Expression) if invoked without curring.
   * @throws EvaluateError exception if the applied expression is an invalid Scheme expression
   */
  def fold(f: BiFunc, identity: Expression): Expression => Expression = {
    case Empty => identity
    case Cons(car, cdr) => car match {
      case Integer(n) => f(Value(n), fold(f, identity)(cdr))
      case Number(n) => f(Value(n), fold(f, identity)(cdr))
      case Func(_, args, g) => g(args) match {
        case Integer(n) => f(Value(n), fold(f, identity)(cdr))
        case Number(n) => f(Value(n), fold(f, identity)(cdr))
        case _ => throw new EvaluateError(g(args) + " is not a number")
      }
      case _ => throw new EvaluateError(car + " is not a number")
    }
    case _ => throw new EvaluateError
  }
}