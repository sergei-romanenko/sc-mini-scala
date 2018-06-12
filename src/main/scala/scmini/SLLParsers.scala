package scmini

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers
import scmini.FGSeparator.{isGNameInRules, startsWithG}

import scala.util.matching.Regex

object SLLParsers extends RegexParsers with ImplicitConversions {

  override val whiteSpace: Regex = """(\s|--.*)+""".r

  def task: SLLParsers.Parser[(Expr, List[Rule])] =
    expr ~ ("where" ~> rules) ^^ { case e ~ rs => (e, rs) }

  def rules: SLLParsers.Parser[List[Rule]] =
    rep(definition)

  def definition: SLLParsers.Parser[Rule] =
    gDef | fDef

  def expr: SLLParsers.Parser[Expr] =
    ctr | call | vrb

  def uid: SLLParsers.Parser[String] =
    """[A-Z]\w*""".r

  def lid: SLLParsers.Parser[String] =
    """[a-z]\w*""".r

  def vrb: SLLParsers.Parser[Var] =
    lid ^^ Var

  def patParams: SLLParsers.Parser[List[String]] =
    opt("(" ~> repsep(lid, ",") <~ ")") ^^ {
      _.getOrElse(Nil)
    }

  def pat: SLLParsers.Parser[Pat] =
    uid ~ patParams ^^ Pat

  def fDef: SLLParsers.Parser[FDef] =
    lid ~ ("(" ~> repsep(lid, ",") <~ ")") ~ ("=" ~> expr <~ ";") ^^ FDef

  def gDef: SLLParsers.Parser[GDef] =
    lid ~ ("(" ~> pat) ~ (rep("," ~> lid) <~ ")") ~ ("=" ~> expr <~ ";") ^^ GDef

  def ctrArgs: SLLParsers.Parser[List[Expr]] =
    opt("(" ~> repsep(expr, ",") <~ ")") ^^ {
      _.getOrElse(Nil)
    }

  def ctr: SLLParsers.Parser[Ctr] =
    uid ~ ctrArgs ^^ Ctr

  def call: SLLParsers.Parser[FCall] =
    lid ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ FCall


  def runParser[T](p: Parser[T], s: String): T = {
    parseAll(p, s) match {
      case Success(result, _) => result
      case NoSuccess(err, next) =>
        val msg =
          "Failed to parse the input task " +
            s"(line ${next.pos.line}, column ${next.pos.column}):\n" +
            s"$err\n${next.pos.longString}"
        sys.error(msg)
    }
  }

  // `parseExpr` and `parseProg` are only used for testing purposes.

  def parseExpr(s: String): Expr = {
    val rawExpr = runParser(expr, s)
    val fg = new FGSeparator(startsWithG)
    fg.fgSepExpr(rawExpr)
  }

  // `parseTask` classifies function names according to their definitions,
  // rather than the first letter of a function name.

  def parseTask(s: String): Task = {
    val (rawExpr, rawRules) = runParser(task, s)
    val fg = new FGSeparator(isGNameInRules(rawRules))
    fg.fgSepTask(rawExpr, rawRules)
  }

}
