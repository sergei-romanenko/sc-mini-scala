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
        (List[Name]() /: call.args)(_ ++ _.vmNames)
      case Let((_, e1), e0) =>
        e1.vmNames ++ e0.vmNames
    }

    def vNames: List[Name] =
      e.vmNames.distinct
  }

  val nameSupply: NameSupply =
    Stream.from(1).map("v" + _.toString)

  implicit class NameSupplyImprovements(ns: NameSupply) {
    def \\(contr: Contraction): NameSupply ={
      val vs = contr.pat.params
      ns.filterNot(n => vs.contains(n))
    }
  }
}
