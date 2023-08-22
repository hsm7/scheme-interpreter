package scheme

import scala.annotation.tailrec
import collection.mutable

class Environment {

  private val stack: mutable.Stack[mutable.Map[Symbol, Expression]] = mutable.Stack.empty

  def get(s: Symbol): Expression = stack.find(_.contains(s)).getOrElse(throw new NoSuchElementException(s.symbol))(s)
  def contains(s: Symbol): Boolean = stack.exists(_.contains(s))
  def put(s: Symbol, expr: Expression): Unit = stack.top.put(s, expr)
  def create(): Unit = stack.push(mutable.Map.empty)
  def destroy(): Unit = stack.pop()
  def size: Int = stack.size

  @tailrec
  final def bind(params: Expression, args: Expression): Unit = params match {
    case Empty => Empty()
    case Cons(car, cdr) => car match {
      case Symbol(s) => args match {
        case Cons(h, t) =>
          put(Symbol(s), h)
          bind(cdr, t)
      }
    }
  }
}

object Environment {

  implicit lazy val global: Environment = {
    val global = new Environment
    global.create()
    val params: Cons = Cons(Symbol("x"), Cons(Symbol("y"), Empty))
    global.put(Symbol("<"),    Lambda(params, Functions.fold(Functions.lt)))
    global.put(Symbol("+"),    Lambda(params, Functions.fold(Functions.add)))
    global.put(Symbol("-"),    Lambda(params, Functions.fold(Functions.subtract)))
    global.put(Symbol("*"),    Lambda(params, Functions.fold(Functions.multiply)))
    global.put(Symbol("/"),    Lambda(params, Functions.fold(Functions.divide)))
    global.put(Symbol("car"),  Lambda(Cons(Symbol("list"), Empty), Functions.car))
    global.put(Symbol("cdr"),  Lambda(Cons(Symbol("list"), Empty), Functions.cdr))
    global.put(Symbol("cons"), Lambda(params, Functions.cons))
    global.put(Symbol("pi"),   Number(3.14))
    global
  }

}