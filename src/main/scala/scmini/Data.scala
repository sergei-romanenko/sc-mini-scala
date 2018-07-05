package scmini

// Expressions

sealed trait Expr

case class Var(name: Name) extends Expr {
  override def toString: String = name
}

sealed trait CFG extends Expr {
  val name: Name
  val args: List[Expr]

  def copy(args: List[Expr]): CFG
}

case class Ctr(name: Name, args: List[Expr]) extends CFG {
  override def copy(args: List[Conf]): Ctr =
    Ctr(name, args)

  override def toString: String =
    if (args.isEmpty) name else name + args.mkString("(", ",", ")")

}

case class FCall(name: Name, args: List[Expr]) extends CFG {
  override def copy(args: List[Conf]): FCall =
    FCall(name, args)

  override def toString: String = name + args.mkString("(", ",", ")")
}

case class GCall(name: Name, args: List[Expr]) extends CFG {
  override def copy(args: List[Conf]): GCall =
    GCall(name, args)

  override def toString: String = name + args.mkString("(", ",", ")")
}

case class Let(binding: (Name, Expr), expr: Expr) extends Expr {
  override def toString: String = {
    val (n, e) = binding
    s"let $n=$e in $expr"
  }
}

// Programs & Tasks

sealed case class Pat(name: Name, params: List[Name]) {
  override def toString: String =
    if (params.isEmpty) name else name + params.mkString("(", ",", ")")
}

sealed trait Rule

case class FDef(name: Name, params: List[Name], expr: Expr) extends Rule {
  override def toString: String =
    s"$name${params.mkString("(", ",", ")")}=$expr;"
}

case class GDef(name: Name, pat: Pat, params: List[Name], expr: Expr) extends Rule {
  lazy val allParams: List[Name] = pat.params ::: params

  override def toString: String =
    s"$name${(pat :: params).mkString("(", ",", ")")}=$expr;"

}

sealed case class Program(fDefs: List[FDef], gDefs: List[GDef]) {
  override def toString: String = {
    val rules: List[Rule] = fDefs ::: gDefs
    rules.mkString("")
  }
}

sealed case class Task(expr: Expr, program: Program) {
  override def toString: String =
    s"$expr where $program"
}

// Contractions

sealed case class Contraction(name: Name, pat: Pat)

// Steps

sealed trait Step[A]

case class Stop[A](a: A) extends Step[A]

case class Transient[A](a: A) extends Step[A]

case class Decompose[A](comp: List[A] => A, as: List[A]) extends Step[A]

case class Variants[A](bs: List[(Contraction, A)]) extends Step[A]

// Step building

trait StepBuilding[A, B] {
  def stop(a: A): B

  def transient(a: A): B

  def decompose(comp: List[A] => A, as: List[A]): B

  def variants(bs: List[(Contraction, A)]): B
}

trait BasicStepBuilding[A] extends StepBuilding[A, Step[A]] {
  def stop(a: A): Step[A] = Stop(a)

  def transient(a: A): Step[A] = Transient(a)

  def decompose(comp: List[A] => A, as: List[A]): Step[A] = Decompose(comp, as)

  def variants(bs: List[(Contraction, A)]) = Variants(bs)
}

// Edges

sealed trait Edge[A]

case class ETransient[A](graph: Node[A]) extends Edge[A]

case class EDecompose[A](comp: List[A] => A, graphs: List[Graph[A]]) extends Edge[A]

case class EVariants[A](variants: List[(Contraction, Graph[A])]) extends Edge[A]

case class EFold[A](graph: Graph[A], renaming: Renaming) extends Edge[A]

// Graphs

sealed trait Graph[A] {
  val label: A
}

object Graph {

  case class Leaf[A](a: A) extends Graph[A] {
    override val label: A = a
  }

  private
  class Branch_%[A](val a: A, val edge: () => Edge[A]) extends Graph[A] {
    override val label: A = a
  }

  object Branch {

    def apply[A](a: A, edge: => Edge[A]): Graph[A] = {
      lazy val edgeVal = edge
      new Branch_%(a, () => edgeVal)
    }

    def unapply[A](arg: Branch_%[A]): Option[(A, Edge[A])] =
      Some(arg.a, arg.edge())
  }

}
