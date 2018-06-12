package scmini

import org.scalatest.FunSuite


class SLLTests extends FunSuite {

  def assertToStr(t: Expr, s: String): Unit = {
    assert(t.toString === s)
  }

  test(testName = "Expr.toString 101 StrVarAndCall") {
    assertToStr(Var("x"), "x")
    assertToStr(Ctr("A", List(Var("x"), Var("y"))), "A(x,y)")
    assertToStr(Ctr("C", Nil), "C")
    assertToStr(FCall("fX", List(Var("x"), Var("y"))), "fX(x,y)")
    assertToStr(GCall("gX", List(Var("x"), Var("y"))), "gX(x,y)")
  }

  test(testName = "Expr.toString 102 StrLet") {
    assertToStr(Let(("x", Var("y")), Var("x")),
      "let x=y in x")
  }

  test(testName = "Rule.toString 103 StrRule") {
    assert(FDef("f", List("x", "y"), Var("y")).toString
      === "f(x,y)=y;")
    assert(GDef("g", Pat("C", List("x")), List("y"), Var("y")).toString
      === "g(C(x),y)=y;")
    assert(GDef("g", Pat("C", Nil), List("y"), Var("y")).toString
      === "g(C,y)=y;")
    assert(GDef("g", Pat("C", Nil), Nil, Ctr("C", Nil)).toString
      === "g(C)=C;")
  }

  test(testName = "Program.toString 104 StrProgram") {
    assert(List(
      FDef("f", Nil, Ctr("A", Nil)),
      FDef("f1", Nil, Ctr("A1", Nil))).mkString("")
      === "f()=A;f1()=A1;")
    assert(List(
      GDef("g", Pat("C", Nil), Nil, Ctr("A", Nil)),
      GDef("g1", Pat("C", Nil), List("x"), Ctr("A", Nil)),
      GDef("g2", Pat("C", List("x")), Nil, Ctr("A", Nil))).mkString("")
      === "g(C)=A;g1(C,x)=A;g2(C(x))=A;")
  }

  test(testName = "Expr.equals 201 Eq") {
    assert(Var("x") ==  Var("x"))
    assert(Var("x") !=  Var("y"))
    assert(Ctr("A", Nil) == Ctr("A", Nil))
    assert(Ctr("A", Nil) != Ctr("B", Nil))
    assert(Nil ==  Nil)
    assert(List(Var("x")) ==  List(Var("x")))
    assert(List(Var("x")) != List(Var("y")))
    assert(List(Var("x")) != List(Var("x"), Var("z")))
    assert(Ctr("A", List(Var("x"))) == Ctr("A", List(Var("x"))))
    assert(Ctr("A", List(Var("x"))) != Ctr("A", List(Var("y"))))
  }

}
