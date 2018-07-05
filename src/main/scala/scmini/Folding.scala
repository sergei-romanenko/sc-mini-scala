package scmini

import scmini.DataUtil._
import scmini.Graph._

object Folding {

  def foldTree(t: Tree[Conf]): Graph[Conf] =
    fixTree(tieKnot(Nil))(t)

  def tieKnot(h: List[Node[Conf]])(g: Node[Conf])(t: Tree[Conf]): Graph[Conf] =
    t match {
      case Leaf(e) => Leaf(e)
      case Branch(e, edge) =>
        if (e.isFGCall) {
          (for (k <- g :: h; r <- findRenaming(k.label, e)) yield (k, r)) match {
            case Nil =>
              fixTree(tieKnot(g :: h))(t)
            case (k, r) :: _ =>
              Branch(e, EFold(k, r))
          }
        } else
          fixTree(tieKnot(g :: h))(t)
    }

  def fixTree[A](f: Node[A] => Tree[A] => Graph[A]): Tree[A] => Graph[A] = {
    case Leaf(a) => Leaf(a)
    case Branch(a, edge) => edge match {
      case ETransient(graph) =>
        lazy val l: Graph[A] = Branch(a, ETransient(f(l)(graph)))
        l
      case EDecompose(comp, graphs) =>
        lazy val l: Graph[A] = Branch(a, EDecompose(comp, graphs.map(f(l))))
        l
      case EVariants(variants) =>
        lazy val l: Graph[A] = Branch(a, EVariants(variants.map {
          case (c, g) => (c, f(l)(g))
        }))
        l
      case EFold(graph, renaming) =>
        sys.error("fixTree: EFold")
    }
  }
}
