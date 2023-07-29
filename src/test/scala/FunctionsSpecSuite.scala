import org.scalatest.funspec.AnyFunSpec
import scheme.Interpreter.EvaluateError
import scheme.{Cons, Empty, Functions, Integer, Number, Str, Symbol}

class FunctionsSpecSuite extends AnyFunSpec{

  describe("fold function") {
    val exp1 = Cons(Integer(7), Empty)
    val exp2 = Cons(Integer(8), Cons(Integer(7), Empty))
    val exp3 = Cons(Number(7), Empty)
    val exp4 = Cons(Number(8), Cons(Number(7), Empty))
    val exp5 = Cons(Number(8), Cons(Integer(7), Empty))
    val invalid = Cons(Symbol("^"), Cons(Str("Seven"), Empty))

    it("should evaluate empty expressions") {
      assert(Functions.fold(Functions.add, Integer(0))(Empty).toString == "0")
    }
    it("should evaluate integer expressions") {
      assert(Functions.fold(Functions.add, Integer(0))(exp1).toString == "7")
      assert(Functions.fold(Functions.subtract, Integer(0))(exp2).toString == "1")
    }
    it("should evaluate number expressions") {
      assert(Functions.fold(Functions.add, Number(0))(exp3).toString == "7.0")
      assert(Functions.fold(Functions.subtract, Number(0))(exp4).toString == "1.0")
    }
    it("should evaluate mixed expressions") {
      assert(Functions.fold(Functions.add, Number(0))(exp5).toString == "15.0")
      assert(Functions.fold(Functions.subtract, Integer(0))(exp5).toString == "1.0")
    }
    it("should throw evaluation error exception") {
      assertThrows[EvaluateError](Functions.fold(Functions.add, Number(0))(invalid))
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

  }
