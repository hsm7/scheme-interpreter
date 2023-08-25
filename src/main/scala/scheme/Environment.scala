package scheme

import scala.annotation.tailrec
import collection.mutable

class Environment(private val stack: mutable.Stack[mutable.Map[Symbol, Expression]]) {

  def get(s: Symbol): Expression = stack.find(_.contains(s)).get(s)
  def put(s: Symbol, expr: Expression): Unit = stack.top.put(s, expr)
  def size: Int = stack.size
  def push(map: mutable.Map[Symbol, Expression]): Unit = stack.push(map)
  def pop: mutable.Map[Symbol, Expression] = stack.pop

}

object Environment {

  implicit lazy val global: Environment = {
    val params: Cons = Cons(Symbol("x"), Cons(Symbol("y"), Empty))
    new Environment(mutable.Stack(mutable.Map[Symbol, Expression](
      Symbol("<") -> Lambda(params, Functions.fold(Functions.lt)),
      Symbol("+") -> Lambda(params, Functions.fold(Functions.add)),
      Symbol("-") -> Lambda(params, Functions.fold(Functions.subtract)),
      Symbol("*") -> Lambda(params, Functions.fold(Functions.multiply)),
      Symbol("/") -> Lambda(params, Functions.fold(Functions.divide)),
      Symbol("car") -> Lambda(Cons(Symbol("list"), Empty), Functions.car),
      Symbol("cdr") -> Lambda(Cons(Symbol("list"), Empty), Functions.cdr),
      Symbol("cons") -> Lambda(params, Functions.cons),
      Symbol("pi") -> Number(3.14)
    )))
  }

  def bind(params: Expression, args: Expression): mutable.Map[Symbol, Expression] = {
    @tailrec
    def _bind(params: Expression, args: Expression, map: mutable.Map[Symbol, Expression]):
    mutable.Map[Symbol, Expression] = params match {
      case Empty => map
      case Cons(car, cdr) => car match {
        case Symbol(s) => args match {
          case Cons(h, t) => _bind(cdr, t, map += (Symbol(s) -> h))
        }
      }
    }
    _bind(params, args, mutable.Map.empty[Symbol, Expression])
  }
}