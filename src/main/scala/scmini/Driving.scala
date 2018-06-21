package scmini

import scmini.DataUtil._
import scmini.Interpreter.evalStep

sealed case class DriveMachine(p: Program) {
  def driveStep: Machine[Conf] = ns => {
    case e@Var(name) =>
      Stop(e)
    case GCall(gname, Var(vname) :: args) =>
      Variants(p.gDefList(gname).map(scrutinize(ns, vname, args)))
    case GCall(gname, arg :: args) if arg.isFGCall =>
      driveStep(ns)(arg) match {
        case Transient(opat, e1) =>
          Transient(opat, GCall(gname, e1 :: args))
        case Variants(bs) =>
          Variants(bs.map { case (c1, e1) => (c1, GCall(gname, e1 :: args)) })
        case _ => sys.error("driveStep")
      }
    case Let((name, expr), body) =>
      Decompose({ case List(e1, e2) => e2 /#/ List((name, e1)) },
        List(expr, body))
    case e =>
      evalStep(p)(e)
  }

  def scrutinize(ns: NameSupply, name: Name, args: List[Expr])
                (gd: GDef): (Contraction, Expr) = {
    var cparams = gd.pat.params
    var fresh = ns.take(cparams.length).toList
    var subst = (cparams ::: gd.params).zip(fresh.map(Var) ::: args)
    (Contraction(name, Pat(gd.pat.name, fresh)), gd.expr /#/ subst)
  }
}

object Driving {
  def driveMachine(p: Program): Machine[Conf] =
    DriveMachine(p).driveStep
}
