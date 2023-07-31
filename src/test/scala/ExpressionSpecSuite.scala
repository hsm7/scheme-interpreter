import org.scalatest.funspec.AnyFunSpec
import scheme.{Bool, Empty, Expression, Integer, Number, Str}

class ExpressionSpecSuite extends AnyFunSpec {

  describe("Expression.evaluate") {
    val exp1 = Expression.parse("(* 4 (- 5 -8))")
    val exp2 = Expression.parse("(+ 1 6.1 (+ 5.4 1.5) (+ 2 3))")


    it("should evaluate simple expressions") {
      assert(Expression.evaluate(Integer(7)).toString == "7")
      assert(Expression.evaluate(Number(7.7)).toString == "7.7")
      assert(Expression.evaluate(Bool(true)).toString == "#t")
      assert(Expression.evaluate(Str("String")).toString == "String")
    }
    it("should evaluate math expressions") {
      assert(Expression.evaluate(exp1).toString == "52")
      assert(Expression.evaluate(exp2).toString == "19.0")
    }
  }

  describe("Expression.parse") {

    it("should parse empty expression") {
      assert(Expression.parse("()") == Empty)
    }
    it("should parse integer expression") {
      assert(Expression.parse("7").toString == "7")
      assert(Expression.parse("-7").toString == "-7")
    }
    it("should parse number expression") {
      assert(Expression.parse("7.0").toString == "7.0")
      assert(Expression.parse("-7.0").toString == "-7.0")
    }
    it("should parse boolean expression") {
      assert(Expression.parse("#t").toString == "#t")
      assert(Expression.parse("#f").toString == "#f")
    }
    it("should parse string expression") {
      assert(Expression.parse("\"a valid scheme string\"").toString == "a valid scheme string")
      assert(Expression.parse("\"a 'SCHEME' string: #1\"").toString == "a 'SCHEME' string: #1")
    }
    it("should parse symbol expression") {
      assert(Expression.parse("x").toString == "x")
      assert(Expression.parse("var1").toString == "var1")
      assert(Expression.parse("!").toString == "!")
      assert(Expression.parse("?").toString == "?")
    }
    describe("list expressions") {
      it("should parse list of numbers") {
        assert(Expression.parse("(1 2 3 4 5)").toString == "(1 2 3 4 5)")
      }
      it("should parse nested list of numbers") {
        assert(Expression.parse("(1 2.0 (3.5 4) 5)").toString == "(1 2.0 (3.5 4) 5)")
      }
      it("should parse list of any value") {
        assert(Expression.parse("(1 #t ((3.9) \"str\") +)").toString == "(1 #t ((3.9) str) +)")
      }
    }
    describe("function expressions") {
      it("should parse simple math functions") {
        assert(Expression.parse("(+ 4 5)").toString == "(+ 4 5)")
        assert(Expression.parse("(/ 10.0 5)").toString == "(/ 10.0 5)")
      }
      it("should parse nested math functions") {
        assert(Expression.parse("(* 4 (- 5 -8))").toString == "(* 4 (- 5 -8))")
      }
      it("should parse more math functions") {
        assert(Expression.parse("(+ 1 6.1 (+ 5.4 1.5) (+ 2 3))").toString == "(+ 1 6.1 (+ 5.4 1.5) (+ 2 3))")
      }
      it("should parse list functions") {
        assert(Expression.parse("(car (4 5))").toString == "(car (4 5))")
        assert(Expression.parse("(cdr (10.0 5))").toString == "(cdr (10.0 5))")
      }
    }
  }
}
