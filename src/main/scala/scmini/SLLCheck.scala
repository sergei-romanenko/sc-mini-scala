package scmini

import scala.annotation.tailrec
import scala.collection.mutable

class SLLCheck(val task: Task) {

  // A name belongs to one and only one of the sets:
  //   gNames, fNames, pNames, cNames.
  // A variable in a pattern can appear only once.
  // All constructors in the definition of a g-function must be different.
  // A variable appearing in the right hand side must be a parameter.
  // A function name appearing in the right hand side must have a definition.
  // The arities of constructors and functions must be consistent.

  val fDefs: List[FDef] = task.program.fDefs

  val gRules: List[GDef] = task.program.gDefs

  val fNames: List[Name] = fDefs.map(_.name)
  val gNames: List[Name] = gRules.map(_.name)
  val hNames: List[Name] = fNames union gNames
  val fParams: List[Name] = fDefs.flatMap(_.params)
  val gParams: List[Name] = gRules.flatMap(_.allParams)
  val pNames: List[Name] = vExpr(task.expr) union (fParams union gParams)

  // Disjointness of name sets.

  def fgIntersection: Option[Msg] =
    for (f <- fNames.intersect(gNames).headOption) yield
      s"$f is both f- and g-function"

  def hpIntersection: Option[Msg] =
    for (f <- hNames.intersect(pNames).headOption) yield
      s"$f is both a function and a parameter"

  // Collecting variable names.

  def vExpr(expr: Expr): List[Name] = (expr: @unchecked) match {
    case Var(name) =>
      List(name)
    case call: CFG =>
      call.args.flatMap(vExpr)
  }

  // Repeated names.

  @tailrec
  private def repeatedName(names: List[Name]): Option[Name] = names match {
    case Nil => None
    case n :: ns =>
      if (ns.contains(n)) Some(n) else repeatedName(ns)
  }

  def rnFRule(fRule: FDef): Option[Msg] = {
    for (n <- repeatedName(fRule.params))
      yield s"$n is repeated in the parameters of ${fRule.name}"
  }

  def rnGRule(gRule: GDef): Option[Msg] = {
    for (n <- repeatedName(gRule.allParams))
      yield s"$n is repeated in the parameters of ${gRule.name}"
  }

  def rnTask: Option[Msg] =
    fDefs.flatMap(rnFRule).headOption orElse
      gRules.flatMap(rnGRule).headOption

  // Repeated constructor names in g-rules.

  def rcGRules(name: Name): Option[Msg] = {
    val cs = for (r <- gRules if name == r.name) yield r.pat.name
    for (c <- repeatedName(cs))
      yield s"In the definition of $name, $c appears twice in the patterns"
  }

  def rcTask: Option[Msg] =
    gNames.flatMap(rcGRules).headOption

  // A variable must be a parameter.

  def pvFRule(fRule: FDef): Option[Msg] = {
    val ps = fRule.params
    val vs = vExpr(fRule.expr)
    for (v <- vs.find(!ps.contains(_))) yield
      s"In the definition of ${fRule.name}, $v is not a parameter"
  }

  def pvGRule(gRule: GDef): Option[Msg] = {
    val ps = gRule.allParams
    val vs = vExpr(gRule.expr)
    for (v <- vs.find(!ps.contains(_))) yield
      s"In the definition of ${gRule.name}, $v is not a parameter"
  }

  def pvTask: Option[Msg] =
    fDefs.flatMap(pvFRule).headOption orElse
      gRules.flatMap(pvGRule).headOption

  // Collecting function names.

  // We already know that fNames and gNames are disjoint,
  // and all calls to g-functions are marked as GCalls.
  // So, we only need to check that there are rules for FCalls.

  def fExpr(expr: Expr): List[Name] = (expr: @unchecked) match {
    case Var(_) =>
      Nil
    case call: CFG =>
      val fs = call.args.flatMap(fExpr)
      if (call.isInstanceOf[FCall]) call.name :: fs else fs
  }

  // Functions called in expressions must be defined in the program.

  def dExpr(expr: Expr): Option[Name] =
    fExpr(expr).find(!fNames.contains(_))

  def uFRule(fRule: FDef): Option[Msg] =
    for (f <- dExpr(fRule.expr)) yield
      s"In the definition of ${fRule.name}" +
        s", there is a call to an undefined function $f"

  def uGRule(gRule: GDef): Option[Msg] =
    for (f <- dExpr(gRule.expr)) yield
      s"In the definition of ${gRule.name}" +
        s", there is a call to an undefined function $f"

  def uExpr(expr: Expr): Option[Msg] =
    for (f <- dExpr(expr)) yield
      s"In the initial expression, there is a call to an undefined function $f"

  def uTask: Option[Msg] =
    fDefs.flatMap(uFRule).headOption orElse
      (gRules.flatMap(uGRule).headOption orElse
        uExpr(task.expr))

  // The arities of constructors and functions must be consistent.

  def arTask: Option[Msg] =
    CArChecker().caTask(task.expr, fDefs, gRules) orElse
      HArChecker().haTask(task.expr, fDefs, gRules)

  // All checks.

  def checkTask: Option[Msg] =
    fgIntersection orElse (hpIntersection orElse
      (rnTask orElse (rcTask orElse
        (pvTask orElse (uTask orElse arTask)))))
}

object SLLCheck {

  type Msg = String

  def checkTask(task: Task): Option[Msg] = {
    val checker = new SLLCheck(task)
    checker.checkTask
  }
}

// Arities

trait ArChecker {

  val ar: mutable.Map[Name, Int] = mutable.HashMap[Name, Int]()

  def updAr(name: Name, k: Int): Option[Msg] = {
    ar.get(name) match {
      case None =>
        ar += (name -> k)
        None
      case Some(k1) =>
        if (k == k1)
          None
        else Some(s"$name has inconsistent arity: $k and $k1")
    }
  }
}

// Arities of constructors.

case class CArChecker() extends ArChecker {

  def caExpr(expr: Expr): Option[Msg] = (expr: @unchecked) match {
    case Var(name) =>
      None
    case Ctr(name, args) =>
      updAr(name, args.length) orElse caArgs(args)
    case call: CFG =>
      caArgs(call.args)
  }

  def caArgs(args: List[Expr]): Option[Msg] =
    args.flatMap(caExpr).headOption

  def caFRules(rs: List[FDef]): Option[Msg] =
    rs.flatMap(r => caExpr(r.expr)).headOption

  def caGRule(r: GDef): Option[Msg] =
    updAr(r.pat.name, r.pat.params.length) orElse caExpr(r.expr)

  def caGRules(rs: List[GDef]): Option[Msg] =
    rs.flatMap(caGRule).headOption

  def caTask(expr: Expr,
             fRules: List[FDef], gRules: List[GDef]): Option[Msg] =
    caExpr(expr) orElse (caFRules(fRules) orElse caGRules(gRules))
}

// Arities of functions.
// (We already know that fNames and gNames are disjoint.)

case class HArChecker() extends ArChecker {

  def haExpr(expr: Expr): Option[Msg] = (expr: @unchecked) match {
    case Var(name) =>
      None
    case Ctr(name, args) =>
      haArgs(args)
    case call: CFG =>
      updAr(call.name, call.args.length) orElse haArgs(call.args)
  }

  def haArgs(args: List[Expr]): Option[Msg] =
    args.flatMap(haExpr).headOption

  def haFRule(fRule: FDef): Option[Msg] =
    updAr(fRule.name, fRule.params.length) orElse haExpr(fRule.expr)

  def haFRules(rs: List[FDef]): Option[Msg] =
    rs.flatMap(haFRule).headOption

  def haGRule(gRule: GDef): Option[Msg] =
    updAr(gRule.name, gRule.params.length + 1) orElse haExpr(gRule.expr)

  def haGRules(gRules: List[GDef]): Option[Msg] =
    gRules.flatMap(haGRule).headOption

  def haTask(expr: Expr,
             fRules: List[FDef], gRules: List[GDef]): Option[Msg] =
    haExpr(expr) orElse (haFRules(fRules) orElse haGRules(gRules))

}
