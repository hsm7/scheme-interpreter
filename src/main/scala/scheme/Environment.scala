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
    val param: Cons = Cons.from(Symbol("x"))
    val params: Cons = Cons.from(Symbol("x"), Symbol("y"))
    new Environment(mutable.Stack(mutable.Map[Symbol, Expression](
      Symbol("<") -> Lambda(params, Functions.lt),
      Symbol("+") -> Lambda(params, Functions.add),
      Symbol("-") -> Lambda(params, Functions.subtract),
      Symbol("*") -> Lambda(params, Functions.multiply),
      Symbol("/") -> Lambda(params, Functions.divide),
      Symbol("car") -> Lambda(param, Functions.car),
      Symbol("cdr") -> Lambda(param, Functions.cdr),
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
        case Symbol(s) => _bind(cdr, Cons.cdr(args), map += (Symbol(s) -> Cons.car(args)))
      }
    }
    _bind(params, args, mutable.Map.empty[Symbol, Expression])
  }
}