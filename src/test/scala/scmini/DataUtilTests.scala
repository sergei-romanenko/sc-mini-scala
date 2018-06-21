package scmini

import org.scalatest.FunSuite

import TestUtil._
import DataUtil.findRenaming
import SLLParsers.parseExpr
import SLLSamples._

class DataUtilTests extends FunSuite {

  def runFR(e1: String, e2: String, r: String): Unit = {
    val expr1 = parseExpr(e1)
    val expr2 = parseExpr(e2)
    val ren = findRenaming(expr1, expr2)
    assert(ren.toString == r)
  }

  test(testName = "x ~ a") {
    runFR(e1 = "x", e2 = "a",
      r = "Some(List((x,a)))")
  }

  test(testName = "C(x, y) ~ C(a, b)") {
    runFR(e1 = "C(x, y)", e2 = "C(a, b)",
      r = "Some(List((x,a), (y,b)))")
  }

  test(testName = "f(x, y) ~ f(a, b)") {
    runFR(e1 = "f(x, y)", e2 = "f(a, b)",
      r = "Some(List((x,a), (y,b)))")
  }

  test(testName = "g(x, y) ~ g(a, b)") {
    runFR(e1 = "g(x, y)", e2 = "g(a, b)",
      r = "Some(List((x,a), (y,b)))")
  }

  test(testName = "C(x, y) ~ C(a, a)") {
    runFR(e1 = "C(x, y)", e2 = "C(a, a)",
      r = "None")
  }

  test(testName = "C(x, x) ~ C(a, b)") {
    runFR(e1 = "C(x, x)", e2 = "C(a, b)",
      r = "None")
  }

  test(testName = "M(x, N(y, z)) ~ M(x, N(y, z))") {
    runFR(e1 = "M(x, N(y, z))", e2 = "M(x, N(y, z))",
      r = "Some(List((x,x), (y,y), (z,z)))")
  }
}
