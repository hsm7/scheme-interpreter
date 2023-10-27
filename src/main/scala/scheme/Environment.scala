package scheme

import scala.annotation.tailrec
import collection.mutable

class Environment(private val stack: mutable.Stack[mutable.Map[Symbol, Expression]]) {

  /* Retrieve a symbol from the closest scope */
  def get(s: Symbol): Expression = stack.find(_.contains(s)).getOrElse(throw new NoSuchElementException(s.symbol))(s)

  /* Bind a symbol to an expression in the current scope */
  def put(s: Symbol, expr: Expression): Unit = stack.top.put(s, expr)

  /* Number of scopes from the current scope to the global scope */
  def size: Int = stack.size

  /* Create new scope */
  def push(map: mutable.Map[Symbol, Expression]): Unit = stack.push(map)

  /* Remove current scope */
  def pop: mutable.Map[Symbol, Expression] = stack.pop

  /* Update bindings in the closest scope */
  def update(s: Symbol, expr: Expression): Unit = stack.find(_.contains(s)).get.put(s, expr)

  override def toString: String = stack.map(map => map.toString).reduce(_ + ", " + _)
}

object Environment {

  implicit lazy val global: Environment = {
    val param: SList = Symbol("x") :: Empty
    val params: SList = Symbol("x") :: Symbol("y") :: Empty
    
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

  def from(params: SList, args: SList): mutable.Map[Symbol, Expression] = {
    @tailrec
    def _from(params: SList, args: SList, map: mutable.Map[Symbol, Expression]):
    mutable.Map[Symbol, Expression] = params match {
      case Empty => map
      case Cons(s: Symbol, cdr) => _from(cdr, args.cdr, map += s -> args.car)
    }

    _from(params, args, mutable.Map.empty[Symbol, Expression])
  }
}