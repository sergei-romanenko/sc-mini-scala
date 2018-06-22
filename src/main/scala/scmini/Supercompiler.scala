package scmini

import scmini.DataUtil._
import scmini.Deforester.simplify
import scmini.Driving.driveMachine
import scmini.FTreeBuilder.buildFTree
import scmini.Folding.foldTree
import scmini.Generator.residuate

object Supercompiler {

  def supercompile: Task => Task = {
    case Task(e, p) =>
      val t = buildFTree(addPropagation(driveMachine(p)))(e)
      residuate(simplify(foldTree(t)))
  }

  def addPropagation(m: Machine[Conf]): Machine[Conf] = ns => e =>
    propagateContraction(m(ns)(e))

  def propagateContraction: Step[Conf] => Step[Conf] = {
    case Variants(bs) =>
      Variants(bs.map {
        case (c, e) =>
          val subst = List((c.name, Ctr(c.pat.name, c.pat.params.map(Var))))
          (c, e /#/ subst)
      })
    case s => s
  }
}
