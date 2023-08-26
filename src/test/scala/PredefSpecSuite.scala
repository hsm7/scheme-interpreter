import org.scalatest.funspec.AnyFunSpec
import scheme.{Cons, Empty, Number, Predef}

class PredefSpecSuite extends AnyFunSpec{

  describe("math functions") {
    it("should add two numbers") {
      assert(Predef.add(Cons.from(Number(5), Number(10))).toString == "15")
    }
    it("should multiply two numbers") {
      assert(Predef.multiply(Cons.from(Number(5), Number(10))).toString == "50")
    }
    it("should subtract two numbers") {
      assert(Predef.subtract(Cons.from(Number(20), Number(10))).toString == "10")
    }
    it("should divide two numbers") {
      assert(Predef.divide(Cons.from(Number(10), Number(10))).toString == "1")
    }
  }

  describe("list functions") {
    it("should retrieve first element") {
      assert(Predef.car(Cons(Cons(Number(7), Empty), Empty)).toString == "7")
    }
    it("should retrieve an empty list") {
      assert(Predef.car(Cons(Empty, Empty)).toString == "()")
    }
    it("should retrieve cdr elements") {
      assert(Predef.cdr(Cons(Cons.from(Number(7), Number(8)), Empty)).toString == "(8)")
    }
    it("should add new element") {
      assert(Predef.cons(Cons.from(Number(5), Cons.from(Number(7), Number(8)))).toString == "(5 7 8)")
    }
  }

}
