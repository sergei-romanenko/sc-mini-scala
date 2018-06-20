package scmini

object DataUtil {

  def lookup[A, B](key: A, as: List[(A, B)]): B =
    as.find(_._1 == key).get._2

  implicit class ProgramImprovements(p: Program) {
    def fDef(name: Name): FDef =
      p.fDefs.find(_.name == name).get

    def gDef(name: Name, cname: Name): GDef = {
      p.gDefs.find(gd => gd.name == name && gd.pat.name == cname).get
    }
  }

  implicit class ExprImprovements(val e: Expr) {
    def /#/(subst: Subst): Expr = e match {
      case Var(name) => lookup(name, subst)
      case call: CFG =>
        call.copy(call.args.map(_ /#/ subst))
      case Let((name, e1), e0) =>
        Let((name, e1 /#/ subst), e0 /#/ subst)
    }

    def size: Int = e match {
      case Var(name) => 1
      case call: CFG =>
        1 + call.args.map(_.size).sum
      case Let((_, e1), e0) =>
        1 + e1.size + e0.size
    }

    def vmNames: List[Name] = e match {
      case Var(name) =>
        List(name)
      case call: CFG =>
        (List[Name]() /: call.args) (_ ++ _.vmNames)
      case Let((_, e1), e0) =>
        e1.vmNames ++ e0.vmNames
    }

    def vNames: List[Name] =
      e.vmNames.distinct

    def isFGCall: Boolean = e match {
      case FCall(_, _) => true
      case GCall(_, _) => true
      case _ => false
    }
  }

  val nameSupply: NameSupply =
    Stream.from(1).map("v" + _.toString)

  implicit class NameSupplyImprovements(ns: NameSupply) {
    def \\(contr: Contraction): NameSupply = {
      val vs = contr.pat.params
      ns.filterNot(n => vs.contains(n))
    }
  }

  def findRenaming(e1: Expr, e2: Expr): Option[Renaming] = {
    val ops: List[Option[(Name, Name)]] = frn(e1, e2)
    if (ops.contains(None))
      None
    else {
      val ps = ops.flatten.distinct
      val clash1 = ps.groupBy(_._1).values.exists(_.length != 1)
      val clash2 = ps.groupBy(_._2).values.exists(_.length != 1)
      if (clash1 || clash2) None else Some(ps)
    }
  }

  private
  def frn(e1: Expr, e2: Expr): List[Option[(Name, Name)]] =
    (e1, e2) match {
      case (Ctr(n1, args1), Ctr(n2, args2)) if n1 == n2 =>
        (args1, args2).zipped.flatMap(frn)
      case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 =>
        (args1, args2).zipped.flatMap(frn)
      case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 =>
        (args1, args2).zipped.flatMap(frn)
      case (Let((name1, expr1), body1), Let((name2, expr2), body2)) =>
        frn(expr1, expr2) ::: frn(body1, body2 /#/ List((name1, Var(name2))))
      case (_, _) => List(None)

    }

}
