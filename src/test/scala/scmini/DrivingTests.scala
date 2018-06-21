package scmini

import org.scalatest.FunSuite

import scmini.DataUtil.nameSupply
import scmini.Driving.driveMachine
import scmini.TestUtil.mkTask
import scmini.SLLSamples._

class DrivingTests extends FunSuite {

  def run(e: String, p: String, r: String): Unit = {
    val task = mkTask(e, p)
    val result = driveMachine(task.program)(nameSupply)(task.expr)
    assert(result.toString == r)
  }

  // Variants

  test(testName = "odd(add(x, mult(x, S(x))))") {
    run("odd(add(x, mult(x, S(x))))", progArith,
      "Variants(List((Contraction(x,Z),odd(mult(x,S(x)))), " +
        "(Contraction(x,S(v1)),odd(S(add(v1,mult(x,S(x))))))))")

  }

  // Transient step

  test(testName = "odd(S(add(v1, mult(x, S(x)))))") {
    run("odd(S(add(v1, mult(x, S(x)))))", progArith,
      "Transient(Some(S(x)),even(add(v1,mult(x,S(x)))))")
  }

}
