package scmini

import DataUtil._

object Interpreter {

  def eval(p: Program)(e: Expr): Expr =
    evalStep(p)(e) match {
      case Stop(e1) => e1
      case Transient(_, e1) =>
        eval(p)(e1)
      case Decompose(comp, es) =>
        comp(es.map(eval(p)))
      case Variants(bs) =>
        sys.error("eval: Variants")
    }

  def evalStep(p: Program): Expr => Step[Expr] = {
    case Var(name) =>
      sys.error("evalStep: Var")
    case Ctr(name, args) =>
      Decompose(Ctr(name, _), args)
    case FCall(name, args) =>
      val FDef(_, params, body) = p.fDef(name)
      Transient(None, body /#/ params.zip(args))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val GDef(_, pat, params, body) = p.gDef(name, cname)
      val subst = (pat.params ++ params).zip(cargs ++ args)
      Transient(Some(pat), body /#/ subst)
    case GCall(name, arg :: args) =>
      evalStep(p)(arg) match {
        case Transient(contr, arg1) =>
          Transient(contr, GCall(name, arg1 :: args))
        case _ => sys.error("evalStep: GCall 1")
      }
    case GCall(name, Nil) =>
      sys.error("evalStep: GCall 2")
    case Let(binding, expr) =>
      sys.error("evalStep: Let")
  }

}

/*
-- OLD STUFF FURTHER --

int :: Program -> Expr -> Expr
int p = until isValue (intStep p)

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) =
    Ctr name (values ++ (intStep p x : xs)) where
        (values, x : xs) = span isValue args

intStep p (FCall name args) =
    body // zip vs args where
        (FDef _ vs body) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args)) =
    body // zip (cvs ++ vs) (cargs ++ args) where
        (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (e:es)) =
    GCall gname (intStep p e : es)

intStep p (Let (x, e1) e2) =
    e2 // [(x, e1)]

sllRun :: Task -> Env -> Value
sllRun (e, program) env = int program (e // env)

sllTrace :: Task -> Subst -> (Value, Integer)
sllTrace (e, prog) s = intC prog (e // s)

intC :: Program -> Expr -> (Expr, Integer)
intC p e = until t f (e, 0) where
    t (e, n) = isValue e
    f (e, n) = (intStep p e, n + 1)

*/