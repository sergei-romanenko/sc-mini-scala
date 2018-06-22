package scmini

import scmini.Driving.driveMachine
import scmini.FTreeBuilder.buildFTree
import scmini.Folding.foldTree
import scmini.Generator.residuate
import scmini.Graph.{leaf, branch}

object Deforester {

  def deforest: Task => Task = {
    case Task(e, p) =>
      val t = buildFTree(driveMachine(p))(e)
      residuate(simplify(foldTree(t)))
  }

  def simplify(graph: Graph[Conf]): Graph[Conf] = graph match {
    case branch(e, ETransient(opat, g)) if isBase(e)(g) =>
      branch(e, ETransient(opat, simplify(g)))
    case branch(e, ETransient(opat, g)) =>
      simplify(g)
    case branch(e, EDecompose(comp, gs)) =>
      branch(e, EDecompose(comp, gs.map(simplify)))
    case branch(e, EVariants(cgs)) =>
      branch(e, EVariants(cgs.map { case (c1, g1) => (c1, simplify(g1)) }))
    case _ =>
      graph
  }

  def isBase(e: Conf): Graph[Conf] => Boolean = {
    case leaf(_) => false
    case branch(_, edge) => edge match {
      case ETransient(opat, g) =>
        isBase(e)(g)
      case EDecompose(comp, gs) =>
        gs.exists(isBase(e))
      case EVariants(cgs) =>
        cgs.exists {case (_, g) => isBase(e)(g) }
      case EFold(g, _) =>
        e == g.label
    }
  }

}
