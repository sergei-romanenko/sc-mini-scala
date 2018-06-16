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
      case Ctr(name, args) =>
        Ctr(name, args.map(_ /#/ subst))
      case FCall(name, args) =>
        FCall(name, args.map(_ /#/ subst))
      case GCall(name, args) =>
        GCall(name, args.map(_ /#/ subst))
      case Let((name, e1), e0) =>
        Let((name, e1 /#/ subst), e0 /#/ subst)
    }
  }

}
