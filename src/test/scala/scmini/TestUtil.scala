package scmini

import scmini.SLLCheck.checkTask
import scmini.SLLParsers.parseTask

object TestUtil {

  def mkTask(e: String, p: String): Task = {
    val t = e + " where " + p
    val task = parseTask(t)
    assert(checkTask(task).isEmpty)
    task
  }
}
