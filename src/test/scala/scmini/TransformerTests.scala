package scmini

import org.scalatest.FunSuite

import TestUtil._
import Transformer.transform
import Deforester.deforest
import SLLSamples._

class TransformerTests extends FunSuite {

  def runT(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val result = transform(task)
    assert(result.toString == r)
  }

  def runD(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val result = deforest(task)
    assert(result.toString == r)
  }

  // even(sqr(x))

  test(testName = "even(sqr(x)) - transformer") {
    runT("even(sqr(x))", progArith,
      "ff1(x) where " +
        "ff1(x)=gg2(x);" +
        "ff3()=True;" +
        "ff6()=True;" +
        "ff7(v2,v1,x)=gg8(v2,v1,x);" +
        "ff10()=False;" +
        "ff12(v4,v3,x)=gg13(v4,v3,x);" +
        "ff15()=True;" +
        "ff16(v3,v1,x)=gg17(v3,v1,x);" +
        "ff19()=True;" +
        "gg2(Z,x)=ff3();gg2(S(v1),x)=gg4(x,v1);" +
        "gg4(Z,x,v1)=gg5(v1,x);gg4(S(v2),x,v1)=ff7(v2,v1,x);" +
        "gg5(Z,x)=ff6();gg5(S(v2),x)=gg4(x,v2);" +
        "gg8(Z,v1,x)=gg9(v1,x);gg8(S(v3),v1,x)=ff16(v3,v1,x);" +
        "gg9(Z,x)=ff10();gg9(S(v3),x)=gg11(x,v3);" +
        "gg11(Z,x,v3)=gg9(v3,x);gg11(S(v4),x,v3)=ff12(v4,v3,x);" +
        "gg13(Z,v3,x)=gg14(v3,x);gg13(S(v5),v3,x)=ff7(v5,v3,x);" +
        "gg14(Z,x)=ff15();gg14(S(v5),x)=gg4(x,v5);" +
        "gg17(Z,v1,x)=gg18(v1,x);gg17(S(v4),v1,x)=ff7(v4,v1,x);" +
        "gg18(Z,x)=ff19();gg18(S(v4),x)=gg4(x,v4);"
    )
  }

  test(testName = "even(sqr(x)) - deforester") {
    runD("even(sqr(x))", progArith,
      "gg1(x) where " +
        "ff4(v2,v1,x)=gg5(v2,v1,x);" +
        "gg1(Z,x)=True;gg1(S(v1),x)=gg2(x,v1);" +
        "gg2(Z,x,v1)=gg3(v1,x);gg2(S(v2),x,v1)=ff4(v2,v1,x);" +
        "gg3(Z,x)=True;gg3(S(v2),x)=gg2(x,v2);" +
        "gg5(Z,v1,x)=gg6(v1,x);gg5(S(v3),v1,x)=gg10(v3,v1,x);" +
        "gg6(Z,x)=False;gg6(S(v3),x)=gg7(x,v3);" +
        "gg7(Z,x,v3)=gg6(v3,x);gg7(S(v4),x,v3)=gg8(v4,v3,x);" +
        "gg8(Z,v3,x)=gg9(v3,x);gg8(S(v5),v3,x)=ff4(v5,v3,x);" +
        "gg9(Z,x)=True;gg9(S(v5),x)=gg2(x,v5);" +
        "gg10(Z,v1,x)=gg11(v1,x);gg10(S(v4),v1,x)=ff4(v4,v1,x);" +
        "gg11(Z,x)=True;gg11(S(v4),x)=gg2(x,v4);"
    )
  }

  // KMP

  test(testName = "KMP - transformer") {
    runT("match(Cons(A, Cons(A, Cons(B, Nil))), s)", progKMP,
      "ff1(s) where " +
        "ff1(s)=ff2(s);" +
        "ff2(s)=gg3(s);" +
        "ff4(v1,v2,s)=gg5(v1,v2,s);" +
        "ff6(v2,s)=ff7(v2,s);" +
        "ff7(v2,s)=gg8(v2,s);" +
        "ff9(v3,v4,s)=gg10(v3,v4,s);" +
        "ff11(v4,s)=ff12(v4,s);" +
        "ff12(v4,s)=gg13(v4,s);" +
        "ff14(v5,v6,s)=gg15(v5,v6,s);" +
        "ff16(v6,s)=gg17(s);" +
        "ff18(v6,s)=ff19(v6,s);" +
        "ff19(v6,s)=True;" +
        "ff20(v4,s)=gg21(s);" +
        "ff22(v2,s)=gg23(s);" +
        "gg3(Nil,s)=False;gg3(Cons(v1,v2),s)=ff4(v1,v2,s);" +
        "gg5(A,v2,s)=ff6(v2,s);gg5(B,v2,s)=ff22(v2,s);" +
        "gg8(Nil,s)=False;gg8(Cons(v3,v4),s)=ff9(v3,v4,s);" +
        "gg10(A,v4,s)=ff11(v4,s);gg10(B,v4,s)=ff20(v4,s);" +
        "gg13(Nil,s)=False;gg13(Cons(v5,v6),s)=ff14(v5,v6,s);" +
        "gg15(A,v6,s)=ff16(v6,s);gg15(B,v6,s)=ff18(v6,s);" +
        "gg17(Nil)=False;gg17(Cons(v7,v8))=ff2(v8);" +
        "gg21(Nil)=False;gg21(Cons(v5,v6))=ff2(v6);" +
        "gg23(Nil)=False;gg23(Cons(v3,v4))=ff2(v4);"
    )
  }

  test(testName = "KMP - deforester") {
    runD("match(Cons(A, Cons(A, Cons(B, Nil))), s)", progKMP,
      "ff1(s) where " +
        "ff1(s)=gg2(s);" +
        "gg2(Nil,s)=False;gg2(Cons(v1,v2),s)=gg3(v1,v2,s);" +
        "gg3(A,v2,s)=gg4(v2,s);gg3(B,v2,s)=gg10(s);" +
        "gg4(Nil,s)=False;gg4(Cons(v3,v4),s)=gg5(v3,v4,s);" +
        "gg5(A,v4,s)=gg6(v4,s);gg5(B,v4,s)=gg9(s);" +
        "gg6(Nil,s)=False;gg6(Cons(v5,v6),s)=gg7(v5,v6,s);" +
        "gg7(A,v6,s)=gg8(s);gg7(B,v6,s)=True;" +
        "gg8(Nil)=False;gg8(Cons(v7,v8))=ff1(v8);" +
        "gg9(Nil)=False;gg9(Cons(v5,v6))=ff1(v6);" +
        "gg10(Nil)=False;gg10(Cons(v3,v4))=ff1(v4);"
    )
  }

}
