package scmini

// Separating f-functions from g-functions.

case class FGSeparator(isGName: Name => Boolean) {

  def fgSepExpr: Expr => Expr = {
    case Var(name) =>
      Var(name)
    case Ctr(name, args) =>
      Ctr(name, args.map(fgSepExpr))
    case FCall(name, args) =>
      val args1 = args.map(fgSepExpr)
      if (isGName(name)) GCall(name, args1) else FCall(name, args1)
    case _ => sys.error("fgSepExpr: unexpected case")
  }

  def fgSepRules(rules: List[Rule]): Program = {
    val fdefs = rules.collect {
      case FDef(name, params, expr) =>
        FDef(name, params, fgSepExpr(expr))
    }
    val gdefs = rules.collect {
      case GDef(name, pat, params, expr) =>
        GDef(name, pat, params, fgSepExpr(expr))
    }
    Program(fdefs, gdefs)
  }

  def fgSepTask(expr: Expr, rules: List[Rule]): Task = {
    Task(fgSepExpr(expr), fgSepRules(rules))
  }
}

object FGSeparator {

  def startsWithG(name: Name): Boolean =
    name.length > 0 && name.charAt(0) == 'g'

  def getGNames(rules: List[Rule]): List[Name] =
    rules.flatMap {
      case FDef(name, params, term) => None;
      case GDef(name, pat, params, term) => Some(name)
    }

  def isGNameInRules(rules: List[Rule]): Name => Boolean = {
    val gNames = getGNames(rules)
    gNames.contains(_)
  }

}
