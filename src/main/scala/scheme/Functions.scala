package scheme

import scheme.Interpreter.EvaluateError

/* Functional utils to construct Scheme function expressions */
object Functions {
  // Binary mapping function of Scheme expressions
  type BiFunc = (Expression, Expression) => Expression

  /* Addition of Scheme integer and number expression */
  def add: BiFunc = (n, m) => n match {
    case Integer(a) => Integer(a) + m
    case Number(a)  => Number(a) + m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Subtraction of Scheme integer and number expression. */
  def subtract: BiFunc = (n, m) => n match {
    case Integer(a) => Integer(a) - m
    case Number(a)  => Number(a) - m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Multiplication of Scheme integer and number expression. */
  def multiply: BiFunc = (n, m) => n match {
    case Integer(a) => Integer(a) * m
    case Number(a)  => Number(a) * m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /* Division of Scheme integer and number expression. */
  def divide: BiFunc = (n, m) => n match {
    case Integer(a) => Integer(a) / m
    case Number(a)  => Number(a) / m
    case _ => throw new EvaluateError(n + " is not a number")
  }

  /**
   * Right folds the curried expression using a binary mapping function.
   * @param f binary function to apply.
   * @param expr expression to fold. Requires either an Empty expression
   *             or a Cons expression.
   * @return an expression obtained by right folding elements of the curried
   *         expression with `f` function. Or return an expression mapping function
   *         (Expression => Expression) if invoked without curring.
   */
  def fold(f: BiFunc): Expression => Expression = expr => {
    def _fold(f: BiFunc, zero: Expression): Expression => Expression = {
      case Empty => zero
      case Cons(car, cdr) => car match {
        case Func(_, args, g) => f(zero, _fold(f, g(args))(cdr))
        case _ => f(zero, _fold(f, car)(cdr))
      }
    }
    expr match {
      case Empty => Empty()
      case Cons(car, cdr) => _fold(f, car)(cdr)
    }
  }
}