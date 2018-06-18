package scmini

import scmini.DataUtil._

case class FTreeBuilder(m: Machine[Conf]) {

  def build(e: Conf): Tree[Conf] =
    bft(nameSupply, e)

  def bft(ns: NameSupply, e0: Conf): Tree[Conf] =
    if (whistle(e0))
      bft(ns.tail, generalize(ns.head, e0))
    else {
      m(ns)(e0) match {
        case Stop(e) => Leaf(e)
        case Transient(ot, e) =>
          Node(e0, ETransient(ot, bft(ns, e)))
        case Decompose(comp, es) =>
          Node(e0, EDecompose(comp, es.map(bft(ns, _))))
        case Variants(bs) =>
          Node(e0, EVariants(
            for ((c, e) <- bs) yield (c, bft(ns \\ c, e))))
      }
    }

  val sizeBound: Int = 40

  def whistle(e: Expr): Boolean = e match {
    case _: Ctr => false
    case call: CFG =>
      !call.args.forall(_.isInstanceOf[Var]) && call.size > sizeBound
    case _ => false
  }

  def generalize(name: Name, e0: Expr): Expr = e0 match {
    case call: CFG =>
      val (e, es) = extractArg(name, call.args)
      Let((name, e), call.copy(es))
    case _ => sys.error("generalize")
  }

  def extractArg(name: Name, es: List[Expr]): (Expr, List[Expr]) = {
    val maxExpr = es.maxBy(e => if (e.isInstanceOf[Var]) 0 else e.size)
    val (vs, w :: ws) = es.span(_ != maxExpr)
    (w, vs ::: Var(name) :: ws)
  }

}
