package scmini

import org.scalatest.FunSuite

import TestUtil._
import Transformer.transform
import SLLSamples._

class TransformerTests extends FunSuite {

  def runT(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val result = transform(task)
    assert(result.toString == r)
  }

  // even(sqr(x))

  test(testName = "even(sqr(x)) - transformer") {
    runT("even(sqr(x))", progArith,
      "ff1(x) where " +
        "ff1(x)=gg2(x);ff2()=True;ff3()=True;ff4(v2,v1,x)=gg5(v2,v1,x);" +
        "ff5()=False;ff6(v4,v3,x)=gg7(v4,v3,x);ff7()=True;" +
        "ff8(v3,v1,x)=gg9(v3,v1,x);ff9()=True;" +
        "gg2(Z,x)=ff2();gg2(S(v1),x)=gg3(x,v1);" +
        "gg3(Z,x,v1)=gg3(v1,x);gg3(S(v2),x,v1)=ff4(v2,v1,x);" +
        "gg3(Z,x)=ff3();gg3(S(v2),x)=gg3(x,v2);" +
        "gg5(Z,v1,x)=gg5(v1,x);gg5(S(v3),v1,x)=ff8(v3,v1,x);" +
        "gg5(Z,x)=ff5();gg5(S(v3),x)=gg6(x,v3);gg6(Z,x,v3)=gg5(v3,x);" +
        "gg6(S(v4),x,v3)=ff6(v4,v3,x);" +
        "gg7(Z,v3,x)=gg7(v3,x);gg7(S(v5),v3,x)=ff4(v5,v3,x);" +
        "gg7(Z,x)=ff7();gg7(S(v5),x)=gg3(x,v5);" +
        "gg9(Z,v1,x)=gg9(v1,x);gg9(S(v4),v1,x)=ff4(v4,v1,x);" +
        "gg9(Z,x)=ff9();gg9(S(v4),x)=gg3(x,v4);")
  }

  // KMP

  test(testName = "KMP - transformer") {
    runT("match(Cons(A, Cons(A, Cons(B, Nil))), s)", progKMP,
      "ff1(s) where " +
        "ff1(s)=ff2(s);ff2(s)=gg3(s);ff3(v1,v2,s)=gg4(v1,v2,s);" +
        "ff4(v2,s)=ff5(v2,s);ff5(v2,s)=gg6(v2,s);ff6(v3,v4,s)=gg7(v3,v4,s);" +
        "ff7(v4,s)=ff8(v4,s);ff8(v4,s)=gg9(v4,s);ff9(v5,v6,s)=gg10(v5,v6,s);" +
        "ff10(v6,s)=gg11(s);ff11(v6,s)=ff12(v6,s);ff12(v6,s)=True;" +
        "ff13(v4,s)=gg14(s);ff14(v2,s)=gg15(s);" +
        "gg3(Nil,s)=False;gg3(Cons(v1,v2),s)=ff3(v1,v2,s);" +
        "gg4(A,v2,s)=ff4(v2,s);gg4(B,v2,s)=ff14(v2,s);" +
        "gg6(Nil,s)=False;gg6(Cons(v3,v4),s)=ff6(v3,v4,s);" +
        "gg7(A,v4,s)=ff7(v4,s);gg7(B,v4,s)=ff13(v4,s);" +
        "gg9(Nil,s)=False;gg9(Cons(v5,v6),s)=ff9(v5,v6,s);" +
        "gg10(A,v6,s)=ff10(v6,s);gg10(B,v6,s)=ff11(v6,s);" +
        "gg11(Nil)=False;gg11(Cons(v7,v8))=ff2(v8);" +
        "gg14(Nil)=False;gg14(Cons(v5,v6))=ff2(v6);" +
        "gg15(Nil)=False;gg15(Cons(v3,v4))=ff2(v4);")
  }
}
