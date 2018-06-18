package scmini

import scmini.DataUtil._

object Generator {

  def residuate(g: Graph[Conf]): Task = {
    val (expr, program, _) = res(nameSupply, Nil, g)
    Task(expr, program)
  }

  // The generation of residual programs

  def res(ns: NameSupply, mp: List[(Conf, Conf)],
          graph: Graph[Conf]): (Conf, Program, NameSupply) = graph match {
    case Leaf(e) =>
      (e, Program(Nil, Nil), ns)
    case Node(e, edge) => edge match {
      case ETransient(_, g) =>
        val vs = e.vNames
        val f1 = "ff" ++ ns.head.substring(1)
        val fcall = FCall(f1, vs.map(Var))
        val (body, Program(fds, gds), ns1) = res(ns.tail, (e, fcall) :: mp, g)
        (fcall, Program(FDef(f1, vs, body) :: fds, gds), ns1)
      case EDecompose(comp, graphs) =>
        val (args, p1, ns1) = resList(ns, mp, graphs)
        (comp(args), p1, ns1)
      case EVariants(cgs) =>
        val vs = e.vNames
        val pv :: vs1 = vs
        val (vs_, vs1_) =
          if (isRepeated(pv, e) & isUsed(pv, cgs)) (pv :: vs, vs) else (vs, vs1)
        val g1 = "gg" ++ ns.head.substring(1)
        val gcall = GCall(g1, vs.map(Var))
        val (bodies, Program(fds, gds), ns1) =
          resList(ns, (e, gcall) :: mp, cgs.map(_._2))
        val pats = cgs.map(cg => cg._1.pat)
        val newGds =
          for((p, b) <- pats zip bodies) yield GDef(g1, p, vs1_, b)
        (gcall, Program(fds, newGds ++ gds), ns1)
      case EFold(base, renaming) =>
        val baseCall = lookup(base, mp)
        val call = baseCall /#/ (for ((x, y) <- renaming) yield (x, Var(y)))
        (call, Program(Nil, Nil), ns)
    }
  }

  // Processes a list of trees.
  // The main goal is to handle the name supply.

  def resList(ns: NameSupply, mp: List[(Conf, Conf)],
              gs: List[Graph[Conf]]): (List[Conf], Program, NameSupply) = ???

  def isRepeated(name: Name, e: Expr): Boolean =
    e.vmNames.count(_ == name) > 1

  def isUsed(vname: Name, cgs: List[(Contraction, Graph[Expr])]): Boolean =
    cgs.exists(_._2.label.vNames.contains(vname))

}