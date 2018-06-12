package scmini

// Expressions

sealed trait Expr

case class Var(name: Name) extends Expr

case class Ctr(name: Name, args: List[Expr]) extends Expr

case class FCall(name: Name, args: List[Expr]) extends Expr

case class GCall(name: Name, args: List[Expr]) extends Expr

case class Let(binding: (Name, Expr), expr: Expr) extends Expr

// Programs & Tasks

sealed case class Pat(name: Name, params: List[Name])

sealed case class GDef(name: Name, pat: Pat, args: List[Name], expr: Expr)

sealed case class FDef(name: Name, params: List[Name], expr: Expr)

sealed case class Program(fdefs: List[FDef], gdefs: List[GDef])

sealed case class Task(expr: Expr, program: Program)

// Contractions

sealed case class Contraction(name: Name, pat: Pat)

// Test results

sealed trait TestResult

case class Match(pat: Pat) extends TestResult

case class EqTest(eq: Boolean) extends TestResult

// Steps

sealed trait Step[A]

case class Transient[A](ot: Option[TestResult], a: A) extends Step[A]

case class Variants[A](bs: List[(Contraction, A)]) extends Step[A]

case class Stop[A](a: A) extends Step[A]

case class Decompose[A](f: List[A] => A, as: List[A]) extends Step[A]

// Edges

sealed trait Edge[A]

case class ETransient[A](ot: Option[TestResult], graph: Graph[A]) extends Edge[A]

case class EVariants[A](variants: List[(Contraction, Graph[A])]) extends Edge[A]

case class EDecompose[A](f: List[A] => A, graphs: List[Graph[A]]) extends Edge[A]

case class EFold[A](graph: Graph[A], renaming: Renaming) extends Edge[A]

// Graphs

sealed trait Graph[A]

case class Node[A](a: A, edge: Edge[A])

case class Leaf[A](a: A)
