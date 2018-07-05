package scmini

import scmini.DataUtil._

sealed case class DriveMachine(prog: Program) extends BasicStepBuilding[Expr] {
  val int_prog = Interpreter(prog)

  def driveStep: Machine[Conf] = ns => {
    case e@Var(name) =>
      stop(e)
    case GCall(gname, Var(vname) :: args) =>
      variants(prog.gDefList(gname).map(scrutinize(ns, vname, args)))
    case GCall(gname, arg :: args) if arg.isFGCall =>
      driveStep(ns)(arg) match {
        case Transient(e1) =>
          transient(GCall(gname, e1 :: args))
        case Variants(bs) =>
          variants(bs.map { case (c1, e1) => (c1, GCall(gname, e1 :: args)) })
        case _ => sys.error("driveStep")
      }
    case Let((name, expr), body) =>
      decompose({ case List(e1, e2) => e2 /#/ List((name, e1)) },
        List(expr, body))
    case e =>
      int_prog.evalStep(e)
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
