package scheme

import scala.annotation.tailrec
import collection.mutable

class Environment(val map: mutable.Map[Symbol, Expression], val parent: Environment) {

    /* Retrieve a symbol from the closest scope */
    def get(s: Symbol): Expression = {
        if (map.contains(s)) map(s)
        else if (parent != null) parent.get(s)
        else throw new NoSuchElementException(s.symbol)
    }

    /* Bind a symbol to an expression in the current scope */
    def put(s: Symbol, expr: Expression): Unit = map.put(s, expr)

    /* Number of scopes from the current scope to the global scope */
    def size: Int =
        if (parent == null) 1
        else 1 + parent.size

    /* Update bindings in the closest scope */
    def update(s: Symbol, expr: Expression): Unit =
        if (map.contains(s)) map.put(s, expr)
        else if (parent != null) parent.update(s, expr)
        else throw new NoSuchElementException(s.symbol)

    override def toString: String = {
        if (parent == null) s"$map"
        else s"$map\n\t${parent.toString}"
    }
}

object Environment {

    val empty: Environment = Environment(mutable.Map.empty, null)

    implicit lazy val global: Environment = {
        val param: SList = Symbol("x") :: Empty
        val params: SList = Symbol("x") :: Symbol("y") :: Empty

        Environment(mutable.Map[Symbol, Expression](
            Symbol("<") -> Procedure(Symbol("<"), params, _ => Predef.lt),
            Symbol("+") -> Procedure(Symbol("+"), params, _ => Predef.add),
            Symbol("-") -> Procedure(Symbol("-"), params, _ => Predef.subtract),
            Symbol("*") -> Procedure(Symbol("*"), params, _ => Predef.multiply),
            Symbol("/") -> Procedure(Symbol("/"), params, _ => Predef.divide),
            Symbol("equal?") -> Procedure(Symbol("equal?"), params, _ => Predef.equal),
            Symbol("car") -> Procedure(Symbol("car"), param, _ => Predef.car),
            Symbol("cdr") -> Procedure(Symbol("cdr"), param, _ => Predef.cdr),
            Symbol("empty?") -> Procedure(Symbol("empty?"), param, _ => Predef.empty),
            Symbol("cons") -> Procedure(Symbol("cons"), params, _ => Predef.cons),
            Symbol("pi") -> Number(3.14)
        ), null)
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

    def apply(map: mutable.Map[Symbol, Expression]): Environment = {
        new Environment(map, global)
    }

    def apply(map: mutable.Map[Symbol, Expression], parent: Environment): Environment = {
        new Environment(map, parent)
    }
}