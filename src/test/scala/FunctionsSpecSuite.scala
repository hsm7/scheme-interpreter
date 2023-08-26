import org.scalatest.funspec.AnyFunSpec
import scheme.{Bool, Cons, Empty, Expression, Functions, Number, Symbol, Procedure}
import scheme.Environment.global

class FunctionsSpecSuite extends AnyFunSpec{

  describe("math functions") {
    it("should add two numbers") {
      assert(Functions.add(Cons.from(Number(5), Number(10))).toString == "15")
    }
    it("should multiply two numbers") {
      assert(Functions.multiply(Cons.from(Number(5), Number(10))).toString == "50")
    }
    it("should subtract two numbers") {
      assert(Functions.subtract(Cons.from(Number(20), Number(10))).toString == "10")
    }
    it("should divide two numbers") {
      assert(Functions.divide(Cons.from(Number(10), Number(10))).toString == "1")
    }
  }

  describe("list functions") {
    it("should retrieve first element") {
      assert(Functions.car(Cons(Cons(Number(7), Empty), Empty)).toString == "7")
    }
    it("should retrieve an empty list") {
      assert(Functions.car(Cons(Empty, Empty)).toString == "()")
    }
    it("should retrieve cdr elements") {
      assert(Functions.cdr(Cons(Cons.from(Number(7), Number(8)), Empty)).toString == "(8)")
    }
    it("should add new element") {
      assert(Functions.cons(Cons.from(Number(5), Cons.from(Number(7), Number(8)))).toString == "(5 7 8)")
    }
  }

  describe("define function") {
    it("should map symbol to number expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Number(7), Empty)
      val expr: Expression = Cons(s, e)
      assert(Functions.define(expr) == Empty())
      assert(global.get(s).toString == "7")
    }
    it("should map symbol to list expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Cons(Number(7), Cons(Number(8), Empty)), Empty)
      val expr: Expression = Cons(s, e)
      assert(Functions.define(expr) == Empty())
      assert(global.get(s).toString == "(7 8)")
    }
    it("should map symbol to function expression in global environment") {
      val s: Symbol = Symbol("+")
      val e: Expression = Cons(Procedure(s, Cons(Symbol("x"), Cons(Symbol("y"), Empty))), Empty)
      val expr: Expression = Cons(s, e)
      assert(Functions.define(expr) == Empty())
      assert(global.get(s).toString == "(+ x y)")
    }
  }

  describe("_if function") {
    it("should accept true if expressions") {
      val add: Expression = Procedure(Symbol("+"), Cons(Number(8), Cons(Number(10), Empty)))
      val mult: Expression = Procedure(Symbol("*"), Cons(Number(8), Cons(Number(10), Empty)))
      val _if: Expression = Bool(true)
      val expr: Expression = Cons(_if, Cons(add, Cons(mult, Empty)))
      assert(Functions._if(expr).toString == "(+ 8 10)")
    }

    it("should accept false if expressions") {
      val add: Expression = Procedure(Symbol("+"), Cons(Number(8), Cons(Number(10), Empty)))
      val mult: Expression = Procedure(Symbol("*"), Cons(Number(8), Cons(Number(10), Empty)))
      val _if: Expression = Bool(false)
      val expr: Expression = Cons(_if, Cons(add, Cons(mult, Empty)))
      assert(Functions._if(expr).toString == "(* 8 10)")
    }
  }

}
