package scheme

import org.scalatest.funspec.AnyFunSpec

class ExpressionSpecSuite extends AnyFunSpec {

    val evaluator: Evaluator[Expression] = implicitly[Evaluator[Expression]]

    describe("Evaluator.evaluate") {
        val exp1 = Parser.parse("(* 4 (- 5 -8))")
        val exp2 = Parser.parse("(+ (+ 5.4 1.5) (* 2 3))")
        val exp3 = Parser.parse("(car (7 8 9))")
        val exp4 = Parser.parse("(car ())")
        val exp5 = Parser.parse("(cdr (7 8 9))")
        val exp6 = Parser.parse("(cons 6 (7 8 9))")
        val exp7 = Parser.parse("(define x 5)")
        val exp8 = Parser.parse("(define y (1 2 3 4))")
        val exp9 = Parser.parse("(if #t (+ 10 6) (- 10 6))")
        val exp10 = Parser.parse("(if #f (+ 10 6) (- 10 6))")

        it("should evaluate if expressions") {
            assert(evaluator.evaluate(exp9).toString == "16")
            assert(evaluator.evaluate(exp10).toString == "4")
        }

        it("should evaluate define expressions") {
            assert(evaluator.evaluate(exp7).toString == "()")
            assert(evaluator.evaluate(Symbol("x")).toString == "5")
            assert(evaluator.evaluate(exp8).toString == "()")
            assert(evaluator.evaluate(Symbol("y")).toString == "(1 2 3 4)")
        }

        it("should evaluate car expressions") {
            assert(evaluator.evaluate(exp3).toString == "7")
            assert(evaluator.evaluate(exp4).toString == "()")
        }

        it("should evaluate cdr expressions") {
            assert(evaluator.evaluate(exp5).toString == "(8 9)")
        }

        it("should evaluate cons expressions") {
            assert(evaluator.evaluate(exp6).toString == "(6 7 8 9)")
        }

        it("should evaluate simple expressions") {
            assert(evaluator.evaluate(Number(7)).toString == "7")
            assert(evaluator.evaluate(Number(7.7)).toString == "7.7")
            assert(evaluator.evaluate(Bool(true)).toString == "#t")
            assert(evaluator.evaluate(Str("String")).toString == "String")
        }
        it("should evaluate math expressions") {
            assert(evaluator.evaluate(exp1).toString == "52")
            assert(evaluator.evaluate(exp2).toString == "12.9")
        }
    }

    describe("Parser.parse") {

        it("should parse empty expression") {
            assert(Parser.parse("()") == Empty)
        }
        it("should parse integer expression") {
            assert(Parser.parse("7").toString == "7")
            assert(Parser.parse("-7").toString == "-7")
        }
        it("should parse number expression") {
            assert(Parser.parse("7.0").toString == "7.0")
            assert(Parser.parse("-7.0").toString == "-7.0")
        }
        it("should parse boolean expression") {
            assert(Parser.parse("#t").toString == "#t")
            assert(Parser.parse("#f").toString == "#f")
        }
        it("should parse string expression") {
            assert(Parser.parse("\"a valid scheme string\"").toString == "a valid scheme string")
            assert(Parser.parse("\"a 'SCHEME' string: #1\"").toString == "a 'SCHEME' string: #1")
        }
        it("should parse symbol expression") {
            assert(Parser.parse("x").toString == "x")
            assert(Parser.parse("var1").toString == "var1")
            assert(Parser.parse("!").toString == "!")
            assert(Parser.parse("?").toString == "?")
        }
        describe("list expressions") {
            it("should parse list of numbers") {
                assert(Parser.parse("(1 2 3 4 5)").toString == "(1 2 3 4 5)")
            }
            it("should parse nested list of numbers") {
                assert(Parser.parse("(1 2.0 (3.5 4) 5)").toString == "(1 2.0 (3.5 4) 5)")
            }
            it("should parse list of any value") {
                assert(Parser.parse("(1 #t ((3.9) \"str\") +)").toString == "(1 #t ((3.9) str) +)")
            }
        }
        describe("function expressions") {
            it("should parse simple math functions") {
                assert(Parser.parse("(+ 4 5)").toString == "(+ 4 5)")
                assert(Parser.parse("(/ 10.0 5)").toString == "(/ 10.0 5)")
            }
            it("should parse nested math functions") {
                assert(Parser.parse("(* 4 (- 5 -8))").toString == "(* 4 (- 5 -8))")
            }
            it("should parse more math functions") {
                assert(Parser.parse("(+ 1 6.1 (+ 5.4 1.5) (+ 2 3))").toString == "(+ 1 6.1 (+ 5.4 1.5) (+ 2 3))")
            }
            it("should parse list functions") {
                assert(Parser.parse("(car (4 5))").toString == "(car (4 5))")
                assert(Parser.parse("(cdr (10.0 5))").toString == "(cdr (10.0 5))")
                assert(Parser.parse("(cons 8 (10.0 5))").toString == "(cons 8 (10.0 5))")
            }
            it("should parse define expressions") {
                assert(Parser.parse("(define x 5)").toString == "(define x 5)")
                assert(Parser.parse("(define x (1 2 3 4))").toString == "(define x (1 2 3 4))")
            }
        }
    }
}
