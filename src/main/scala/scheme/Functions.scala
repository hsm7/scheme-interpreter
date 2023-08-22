package scheme

import scheme.Interpreter.{Environment, EvaluateError}

/* Functional utils to construct Scheme expressions */
object Functions {

  // Binary mapping function of Scheme expressions
  type BiFunc = (Expression, Expression) => Expression

  /** Construct Schem lambda expressions */
  def lambda: Expression => Expression = {
    case Cons(head, tail) => tail match {
      case Cons(h, _) => Lambda(head, _ => h.preprocess.evaluate)
    }
  }

  /** Construct Scheme procedure expressions */
  def symbol(symbol: Symbol, expr: Expression): Expression = Environment.get(symbol) match {
    case Lambda(_, _) => Procedure(symbol, expr.preprocess)
    case _ => Cons(symbol, expr.preprocess)
  }

  /** Scheme if expression */
  def _if: Expression => Expression = {
    case Cons(car, cdr) => car.evaluate match {
      case Bool(s) if s => cdr match {
        case Cons(h, _) => h
      }
      case Bool(s) if !s => cdr match {
        case Cons(_, t) => t match {
          case Cons(h, _) => h
        }
      }
    }
  }

  /** Map a Scheme symbol to an expression in the global environment */
  def define: Expression => Expression = {
    case Empty => Empty()
    case Cons(car, cdr) => car match {
      case Symbol(s) => cdr match {
        case Cons(h, _) => Interpreter.Environment.put(Symbol(s), h.preprocess)
      }
    }
  }

  /* Retrieve car element from a Scheme list */
  def car: Expression => Expression = {
    case Cons(car, _) => car match {
      case Empty => Empty()
      case Cons(car, _) => car
    }
    case exp => throw new EvaluateError(exp + " is not a list")
  }

  /* Retrieve cdr elements from a Scheme list */
  def cdr: Expression => Expression = {
    case Cons(car, _) => car match {
      case Empty => Empty()
      case Cons(_, cdr) => cdr
    }
    case exp => throw new EvaluateError(exp + " is not a list")
  }

  /* Add new element to a Scheme list */
  def cons: Expression => Expression = {
    case Cons(car, cdr) => cdr match {
      case Empty => Cons(car, Empty())
      case Cons(head, _) => Cons(car, head)
    }
    case exp => throw new EvaluateError(exp + " is not a list")
  }

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
        case Lambda(args, g) => f(zero, _fold(f, g(args))(cdr))
        case _ => f(zero, _fold(f, car)(cdr))
      }
    }
    expr match {
      case Empty => Empty()
      case Cons(car, cdr) => _fold(f, car)(cdr)
    }
  }
}