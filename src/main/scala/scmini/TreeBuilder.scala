package scmini

import scmini.DataUtil._
import scmini.Graph._

sealed case class TreeBuilder(m: Machine[Conf]) {

  def buildTree(e: Conf): Tree[Conf] =
    bt(nameSupply, e)

  def bt(ns: NameSupply, e0: Conf): Tree[Conf] =
    m(ns)(e0) match {
      case Stop(e) => leaf(e)
      case Transient(opat, e) =>
        branch(e0, ETransient(opat, bt(ns, e)))
      case Decompose(comp, es) =>
        branch(e0, EDecompose(comp, es.map(bt(ns, _))))
      case Variants(bs) =>
        branch(e0, EVariants(
          for ((c, e) <- bs) yield (c, bt(ns \\ c, e))))
    }
}

object TreeBuilder {
  def buildTree(m: Machine[Conf])(e: Conf): Tree[Conf] =
    TreeBuilder(m).buildTree(e)
}
