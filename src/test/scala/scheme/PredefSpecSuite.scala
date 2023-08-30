package scheme

import org.scalatest.funspec.AnyFunSpec

class PredefSpecSuite extends AnyFunSpec{

  describe("math functions") {
    it("should add two numbers") {
      assert(Predef.add(Number(5) :: Number(10) :: Empty).toString == "15")
    }
    it("should multiply two numbers") {
      assert(Predef.multiply(Number(5) :: Number(10) :: Empty).toString == "50")
    }
    it("should subtract two numbers") {
      assert(Predef.subtract(Number(20) :: Number(10) :: Empty).toString == "10")
    }
    it("should divide two numbers") {
      assert(Predef.divide(Number(10) :: Number(10) :: Empty).toString == "1")
    }
  }

  describe("list functions") {
    it("should retrieve first element") {
      assert(Predef.car((Number(7) :: Empty) :: Empty).toString == "7")
    }
    it("should retrieve an empty list") {
      assert(Predef.car(Empty :: Empty).toString == "()")
    }
    it("should retrieve cdr elements") {
      assert(Predef.cdr((Number(7) :: Number(8) :: Empty) :: Empty).toString == "(8)")
    }
    it("should add new element") {
      assert(Predef.cons(Number(5) :: (Number(7) :: Number(8) :: Empty) :: Empty).toString == "(5 7 8)")
    }
  }

}
