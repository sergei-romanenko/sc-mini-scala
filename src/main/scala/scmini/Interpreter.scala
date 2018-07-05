package scmini

import DataUtil._

sealed case class Interpreter(p: Program) extends BasicStepBuilding[Expr] {

  def eval(maxDepth: Int, e: Expr): Expr = {
    if (maxDepth == 0) {
      throw new StackOverflowError
    }
    evalStep(e) match {
      case Stop(e1) => e1
      case Transient(e1) =>
        eval(maxDepth - 1, e1)
      case Decompose(comp, es) =>
        comp(es.map(eval(maxDepth - 1, _)))
      case Variants(bs) =>
        sys.error("eval: Variants")
    }
  }

  def evalStep: Expr => Step[Expr] = {
    case Var(name) =>
      sys.error("evalStep: Var")
    case Ctr(name, args) =>
      decompose(Ctr(name, _), args)
    case FCall(name, args) =>
      val FDef(_, params, body) = p.fDef(name)
      transient(body /#/ params.zip(args))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val GDef(_, pat, params, body) = p.gDef(name, cname)
      val subst = (pat.params ++ params).zip(cargs ++ args)
      transient(body /#/ subst)
    case GCall(name, arg :: args) =>
      evalStep(arg) match {
        case Transient(arg1) =>
          transient(GCall(name, arg1 :: args))
        case _ => sys.error("evalStep: GCall 1")
      }
    case GCall(name, Nil) =>
      sys.error("evalStep: GCall 2")
    case Let(binding, expr) =>
      sys.error("evalStep: Let")
  }
}
