package scmini

object DataUtil {

  def lookup[A, B](key: A, as: List[(A, B)]): B =
    as.find(_._1 == key).map(_._2).get

  implicit class ProgramImprovements(p: Program) {
    def fDef(name: Name): FDef =
      p.fDefs.find(_.name == name).get

    def gDef(name: Name, cname: Name): GDef = {
      p.gDefs.find(gd => gd.name == name && gd.pat.name == cname).get
    }

    def gDefList(name: Name): List[GDef] = {
      p.gDefs.filter(gd => gd.name == name)
    }
  }

  def kindEq(e1: CFG, e2: CFG): Boolean = (e1, e2) match {
    case (Ctr(_, _), Ctr(_, _)) => true
    case (FCall(_, _), FCall(_, _)) => true
    case (GCall(_, _), GCall(_, _)) => true
    case (_, _) => false
  }

  def shallowEq(e1: CFG, e2: CFG): Boolean =
    kindEq(e1, e2) &&
      e1.name == e2.name &&
      e1.args.length == e2.args.length

  implicit class ExprImprovements(val e: Expr) {
    def /#/(subst: Subst): Expr = e match {
      case Var(name) =>
        subst.find(_._1 == name).map(_._2).getOrElse(Var(name))
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

    def isVar: Boolean = e match {
      case Var(_) => true
      case _ => false
    }

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
      case (Var(name1), Var(name2)) =>
        List(Some(name1, name2))
      case (c1: CFG, c2: CFG) if shallowEq(c1, c2) =>
        (c1.args, c2.args).zipped.flatMap(frn)
      case (Let((name1, expr1), body1), Let((name2, expr2), body2)) =>
        frn(expr1, expr2) ::: frn(body1, body2 /#/ List((name1, Var(name2))))
      case (_, _) => List(None)
    }
}
