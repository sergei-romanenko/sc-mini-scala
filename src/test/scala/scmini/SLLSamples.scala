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

  val progKMP: String =
    """
      | eqSymb(A, y) = eqA(y);
      | eqSymb(B, y) = eqB(y);
      | eqA(A) = True;  eqA(B) = False;
      | eqB(A) = False; eqB(B) = True;
      | if(True, x, y) = x;
      | if(False, x, y) = y;
      | match(p, s) = m(p, s, p, s);
      | m(Nil, ss, op, os) = True;
      | m(Cons(p, pp), ss, op, os) = gx(ss, p, pp, op, os);
      | gx(Nil, p, pp,  op, os) = False;
      | gx(Cons(s, ss), p, pp,  op, os) = if(eqSymb(p, s), m(pp, ss, op, os), gn(os, op));
      | gn(Nil, op) = False;
      | gn(Cons(s, ss), op) = m(op, ss, op, ss);
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
