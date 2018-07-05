package scmini

import scmini.Driving.driveMachine
import scmini.FTreeBuilder.buildFTree
import scmini.Folding.foldTree
import scmini.Generator.residuate
import scmini.Graph.{Leaf, Branch}

object Deforester {

  def deforest: Task => Task = {
    case Task(e, p) =>
      val t = buildFTree(driveMachine(p))(e)
      residuate(simplify(foldTree(t)))
  }

  def simplify(graph: Graph[Conf]): Graph[Conf] = graph match {
    case Branch(e, ETransient(g)) if isBase(e)(g) =>
      Branch(e, ETransient(simplify(g)))
    case Branch(e, ETransient(g)) =>
      simplify(g)
    case Branch(e, EDecompose(comp, gs)) =>
      Branch(e, EDecompose(comp, gs.map(simplify)))
    case Branch(e, EVariants(cgs)) =>
      Branch(e, EVariants(cgs.map { case (c1, g1) => (c1, simplify(g1)) }))
    case _ =>
      graph
  }

  def isBase(e: Conf): Graph[Conf] => Boolean = {
    case Leaf(_) => false
    case Branch(_, edge) => edge match {
      case ETransient(g) =>
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
