import org.scalatest.funspec.AnyFunSpec
import scheme.{Bool, Cons, Empty, Expression, Functions, Integer, Number, Symbol, Procedure}
import scheme.Environment.global

class FunctionsSpecSuite extends AnyFunSpec{

  describe("fold function") {
    val exp1 = Cons(Integer(7), Empty)
    val exp2 = Cons(Integer(8), Cons(Integer(7), Empty))
    val exp3 = Cons(Number(7), Empty)
    val exp4 = Cons(Number(8), Cons(Number(7), Empty))
    val exp5 = Cons(Number(8), Cons(Integer(7), Empty))

    it("should evaluate empty expressions") {
      assert(Functions.fold(Functions.add)(Empty).toString == "()")
    }
    it("should evaluate integer expressions") {
      assert(Functions.fold(Functions.add)(exp1).toString == "7")
      assert(Functions.fold(Functions.subtract)(exp2).toString == "1")
    }
    it("should evaluate number expressions") {
      assert(Functions.fold(Functions.add)(exp3).toString == "7.0")
      assert(Functions.fold(Functions.subtract)(exp4).toString == "1.0")
    }
    it("should evaluate mixed expressions") {
      assert(Functions.fold(Functions.add)(exp5).toString == "15.0")
      assert(Functions.fold(Functions.subtract)(exp5).toString == "1.0")
    }
  }

  describe("math functions") {
    it("should add two numbers") {
      assert(Functions.add(Integer(5), Integer(10)).toString == "15")
    }
    it("should multiply two numbers") {
      assert(Functions.multiply(Number(5), Integer(10)).toString == "50.0")
    }
    it("should subtract two numbers") {
      assert(Functions.subtract(Integer(20), Number(10)).toString == "10.0")
    }
    it("should divide two numbers") {
      assert(Functions.divide(Number(10), Number(10)).toString == "1.0")
    }
  }

  describe("list functions") {
    it("should retrieve first element") {
      assert(Functions.car(Cons(Cons(Integer(7), Empty), Empty)).toString == "7")
    }
    it("should retrieve an empty list") {
      assert(Functions.car(Cons(Empty, Empty)).toString == "()")
    }
    it("should retrieve cdr elements") {
      assert(Functions.cdr(Cons(Cons(Integer(7), Cons(Integer(8), Empty)), Empty)).toString == "(8)")
    }
    it("should add new element") {
      assert(Functions.cons(Cons(Integer(5), Cons(Cons(Integer(7), Cons(Integer(8), Empty)), Empty))).toString == "(5 7 8)")
    }
  }

  describe("define function") {
    it("should map symbol to number expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Number(7), Empty)
      val expr: Expression = Cons(s, e)
      assert(Functions.define(expr) == Empty())
      assert(global.get(s).toString == "7.0")
    }
    it("should map symbol to list expression in global environment") {
      val s: Symbol = Symbol("x")
      val e: Expression = Cons(Cons(Integer(7), Cons(Integer(8), Empty)), Empty)
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
      assert(Functions._if(expr).toString == "(+ 8.0 10.0)")
    }

    it("should accept false if expressions") {
      val add: Expression = Procedure(Symbol("+"), Cons(Number(8), Cons(Number(10), Empty)))
      val mult: Expression = Procedure(Symbol("*"), Cons(Number(8), Cons(Number(10), Empty)))
      val _if: Expression = Bool(false)
      val expr: Expression = Cons(_if, Cons(add, Cons(mult, Empty)))
      assert(Functions._if(expr).toString == "(* 8.0 10.0)")
    }
  }

}
