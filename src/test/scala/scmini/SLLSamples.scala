package scmini

object SLLSamples {

  val progArith: String =
    """
      | add(Z, y) = y;
      | add(S(x), y) = S(add(x, y));
      | mult(Z, y) = Z;
      | mult(S(x), y) = add(y, mult(x, y));
      | sqr(x) = mult(x, x);
      | even(Z) = True;
      | even(S(x)) = odd(x);
      | odd(Z) = False;
      | odd(S(x)) = even(x);
      | addAcc(Z, y) = y;
      | addAcc(S(x), y) = addAcc(x, S(y));
      |
      | lt(Z, y)  = ltS(y);
      | lt(S(x), y) = ltZ(y, x);
      |
      | ltS(Z) = False;
      | ltS(S(x)) = True;
      |
      | ltZ(Z, x) = False;
      | ltZ(S(y), x) = lt(x, y);
      |
      | test(x, y, z) = P(lt(x, z), lt(y, z));
    """.stripMargin

  val progInf: String =
    """
      | infNat() = S(infNat());
      | take(Z, y) = Z;
      | take(S(x), y) = take2(y, x);
      | take2(S(y), x) = S(take(x, y));
    """.stripMargin

  val prog2: String =
    """
      | gEqSymb(A(), y) = gEqA(y);
      | gEqSymb(B(), y) = gEqB(y);
      | gEqA(A()) = True;  gEqA(B()) = False;
      | gEqB(A()) = False; gEqB(B()) = True;
      | gIf(True, x, y) = x;
      | gIf(False, x, y) = y;
      | fMatch(p, s) = gM(p, s, p, s);
      | gM(Nil(), ss, op, os) = True;
      | gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);
      | gX(Nil(), p, pp,  op, os) = False;
      | gX(Cons(s, ss), p, pp,  op, os) = gIf(gEqSymb(p, s), gM(pp, ss, op, os), gN(os, op));
      | gN(Nil(), op) = False;
      | gN(Cons(s, ss), op) = gM(op, ss, op, ss);
    """.stripMargin

  val prog3: String =
    """
      | add(Z, y) = y;
      | add(S(x), y) = S(add(x, y));
      | gDouble(Z) = Z;
      | gDouble(S(x)) = S(S(gDouble(x)));
      | gHalf(Z) = Z;
      | gHalf(S(x)) = gHalf1(x);
      | gHalf1(Z) = Z;
      | gHalf1(S(x)) = S(gHalf(x));
      | gEq(Z, y) = gEqZ(y);
      | gEq(S(x), y) = gEqS(y, x);
      | gEqZ(Z) = True;
      | gEqZ(S(x)) = False;
      | gEqS(Z, x) = False;
      | gEqS(S(y), x) = gEq(x, y);
    """.stripMargin

}
