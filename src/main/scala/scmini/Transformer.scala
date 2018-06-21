package scmini

import scmini.Driving.driveMachine
import scmini.FTreeBuilder.buildFTree
import scmini.Folding.foldTree
import scmini.Generator.residuate

// SLL program transformers

object Transformer {

  def transform: Task => Task = {
    case Task(e, p) =>
      val t = buildFTree(driveMachine(p))(e)
      residuate(foldTree(t))
  }
}
