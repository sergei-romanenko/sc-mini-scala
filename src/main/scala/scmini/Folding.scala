package scmini

import scmini.DataUtil._
import scmini.Graph._

object Folding {

  def foldTree(t: Tree[Conf]): Graph[Conf] =
    fixTree(tieKnot(Nil))(t)

  def tieKnot(h: List[Node[Conf]])(g: Node[Conf])(t: Tree[Conf]): Graph[Conf] =
    t match {
      case leaf(e) => leaf(e)
      case branch(e, edge) =>
        if (e.isFGCall) {
          (for (k <- g :: h; r <- findRenaming(k.label, e)) yield (k, r)) match {
            case Nil =>
              fixTree(tieKnot(g :: h))(t)
            case (k, r) :: _ =>
              branch(e, EFold(k, r))
          }
        } else
          fixTree(tieKnot(g :: h))(t)
    }

  def fixTree[A](f: Node[A] => Tree[A] => Graph[A]): Tree[A] => Graph[A] = {
    case leaf(a) => leaf(a)
    case branch(a, edge) => edge match {
      case ETransient(opat, graph) =>
        lazy val l: Graph[A] = branch(a, ETransient(opat, f(l)(graph)))
        l
      case EDecompose(comp, graphs) =>
        lazy val l: Graph[A] = branch(a, EDecompose(comp, graphs.map(f(l))))
        l
      case EVariants(variants) =>
        lazy val l: Graph[A] = branch(a, EVariants(variants.map {
          case (c, g) => (c, f(l)(g))
        }))
        l
      case EFold(graph, renaming) =>
        sys.error("fixTree: EFold")
    }
  }
}
