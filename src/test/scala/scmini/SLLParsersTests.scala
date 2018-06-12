package scmini

import org.scalatest.FunSuite
import scmini.SLLParsers.parseExpr

class SLLParsersTests extends FunSuite {

  def pExpr(g: String, e: String): Unit = {
    assert(parseExpr(g).toString == e)
  }

  def pTask(g: String, e: String): Unit = {
    assert(SLLParsers.parseTask(g).toString == e)
  }

  test(testName = "parseExpr") {
    pExpr("x", "x")
    pExpr(" -- 111\nx --222\n --333", "x")
    pExpr("C", "C")
    pExpr("C()", "C")
    pExpr("C(x)", "C(x)")
    pExpr("C(x,y)", "C(x,y)")
    pExpr("fX(x,y)", "fX(x,y)")
    pExpr("fX()", "fX()")
    pExpr("gX(x,y)", "gX(x,y)")
    pExpr("gX()", "gX()")
  }

  test(testName = "parseTask") {

    pTask("a where f(x,y) = f(y, x);g(C(x),y) = g(y, x);",
      "a where f(x,y)=f(y,x);g(C(x),y)=g(y,x);")

    pTask("a where g(Z,y)= y;", "a where g(Z,y)=y;")

    pTask("f(x) where f(x) = f(x);",
      "f(x) where f(x)=f(x);")

    pTask("h(Z) where h(Z) = h(Z);",
      "h(Z) where h(Z)=h(Z);")
  }

}
