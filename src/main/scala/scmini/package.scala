package object scmini {

  type Name = String
  type Renaming = List[(Name, Name)]
  type NameSupply = List[Name]

  type Subst = List[(Name, Expr)]

  type Conf = Expr
  type Value = Expr
  type Env = List[(Name, Value)]

  type Tree[A] = Graph[A]
  //type Node[A] = Tree[A]

  type Machine[A] = NameSupply => A => Step[A]
}