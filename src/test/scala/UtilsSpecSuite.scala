import org.scalatest.funspec.AnyFunSpec
import scheme.{Bool, Cons, Empty, Expression, Number, Procedure, Symbol, Utils}
import scheme.Environment.global

class UtilsSpecSuite extends AnyFunSpec {

  describe("define function") {
    it("should map symbol to number expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Number(7), Empty)
      val expr: Expression = Cons(s, e)
      assert(Utils.define(expr) == Empty())
      assert(global.get(s).toString == "7")
    }
    it("should map symbol to list expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Cons(Number(7), Cons(Number(8), Empty)), Empty)
      val expr: Expression = Cons(s, e)
      assert(Utils.define(expr) == Empty())
      assert(global.get(s).toString == "(7 8)")
    }
    it("should map symbol to function expression in global environment") {
      val s: Symbol = Symbol("+")
      val e: Expression = Cons(Procedure(s, Cons(Symbol("x"), Cons(Symbol("y"), Empty))), Empty)
      val expr: Expression = Cons(s, e)
      assert(Utils.define(expr) == Empty())
      assert(global.get(s).toString == "(+ x y)")
    }
  }

  describe("_if function") {
    it("should accept true if expressions") {
      val add: Expression = Procedure(Symbol("+"), Cons(Number(8), Cons(Number(10), Empty)))
      val mult: Expression = Procedure(Symbol("*"), Cons(Number(8), Cons(Number(10), Empty)))
      val _if: Expression = Bool(true)
      val expr: Expression = Cons(_if, Cons(add, Cons(mult, Empty)))
      assert(Utils._if(expr).toString == "(+ 8 10)")
    }

    it("should accept false if expressions") {
      val add: Expression = Procedure(Symbol("+"), Cons(Number(8), Cons(Number(10), Empty)))
      val mult: Expression = Procedure(Symbol("*"), Cons(Number(8), Cons(Number(10), Empty)))
      val _if: Expression = Bool(false)
      val expr: Expression = Cons(_if, Cons(add, Cons(mult, Empty)))
      assert(Utils._if(expr).toString == "(* 8 10)")
    }
  }
}
