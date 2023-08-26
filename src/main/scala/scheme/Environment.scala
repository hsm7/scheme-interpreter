package scheme

import scala.annotation.tailrec
import collection.mutable

class Environment(private val stack: mutable.Stack[mutable.Map[Symbol, Expression]]) {

  def get(s: Symbol): Expression = stack.find(_.contains(s)).get(s)
  def put(s: Symbol, expr: Expression): Unit = stack.top.put(s, expr)
  def size: Int = stack.size
  def push(map: mutable.Map[Symbol, Expression]): Unit = stack.push(map)
  def pop: mutable.Map[Symbol, Expression] = stack.pop
  def update(s: Symbol, expr: Expression): Unit = stack.find(_.contains(s)).get.put(s, expr)
}

object Environment {

  implicit lazy val global: Environment = {
    val param: Cons = Cons.from(Symbol("x"))
    val params: Cons = Cons.from(Symbol("x"), Symbol("y"))
    new Environment(mutable.Stack(mutable.Map[Symbol, Expression](
      Symbol("<") -> Lambda(params, Predef.lt),
      Symbol("+") -> Lambda(params, Predef.add),
      Symbol("-") -> Lambda(params, Predef.subtract),
      Symbol("*") -> Lambda(params, Predef.multiply),
      Symbol("/") -> Lambda(params, Predef.divide),
      Symbol("equal?") -> Lambda(params, Predef.equal),
      Symbol("car") -> Lambda(param, Predef.car),
      Symbol("cdr") -> Lambda(param, Predef.cdr),
      Symbol("empty?") -> Lambda(param, Predef.empty),
      Symbol("cons") -> Lambda(params, Predef.cons),
      Symbol("pi") -> Number(3.14)
    )))
  }

  def from(params: Expression, args: Expression): mutable.Map[Symbol, Expression] = {
    @tailrec
    def _from(params: Expression, args: Expression, map: mutable.Map[Symbol, Expression]):
    mutable.Map[Symbol, Expression] = params match {
      case Empty => map
      case Cons(car, cdr) => car match {
        case Symbol(s) => _from(cdr, Cons.cdr(args), map += (Symbol(s) -> Cons.car(args)))
      }
    }
    _from(params, args, mutable.Map.empty[Symbol, Expression])
  }
}