package scmini

import org.scalatest.FunSuite
import scmini.SLLSamples._
import scmini.TestUtil._

class InterpreterTests extends FunSuite {
  val maxDepth = 100

  def run(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val result = Interpreter(task.program).eval(maxDepth, task.expr)
    assert(result.toString == r)
  }

  def runEx(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val caught =
      intercept[RuntimeException] {
        Interpreter(task.program).eval(maxDepth, task.expr)
      }
    assert(caught.getMessage == r)
  }

  // Evaluating ground expressions.

  test(testName = "add(S(Z), S(Z))") {
    run("add(S(Z), S(Z))", progArith, "S(S(Z))")
  }

  test(testName = "addAcc(S(Z), S(Z))") {
    run("addAcc(S(Z), S(Z))", progArith, "S(S(Z))")
  }

  test(testName = "sqr(S(S(Z)))") {
    run("sqr(S(S(Z)))", progArith, "S(S(S(S(Z))))")
  }

  test(testName = "even(Z)") {
    run("even(Z)", progArith, "True")
  }

  test(testName = "odd(Z)") {
    run("odd(Z)", progArith, "False")
  }

  test(testName = "even(S(Z))") {
    run("even(S(Z))", progArith, "False")
  }

  test(testName = "even(sqr(S(S(Z))))") {
    run("even(sqr(S(S(Z))))", progArith, "True")
  }

  test(testName = "test(Z, S(S(Z)), S(Z))") {
    run("test(Z, S(S(Z)), S(Z))", progArith, "P(True,False)")
  }

  // Non-ground expressions.

  test(testName = "even(S(x))") {
    runEx("even(S(x))", progArith, "evalStep: Var")
  }

  // Infinite data.

  test(testName = "take(S(S(S(Z))), infNat())") {
    run("take(S(S(S(Z))), infNat())", progInf, "S(S(S(Z)))")
  }
}
