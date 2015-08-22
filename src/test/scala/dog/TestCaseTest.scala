package dog

import org.scalatest.FlatSpec
import org.scalatest.DiagrammedAssertions

class TestCaseTest extends FlatSpec with DiagrammedAssertions {

  "apply" should "return value" in {
    val target = TestCase(for {
      a <- TestResult(0)
    }yield a)
    assert(target.run(()) == TestResult(0))
  }

  "Assertion" should "run multple assertion" in {
    val assert0 = AssertionResult.pass(())
    val assert1 = AssertionResult.pass(())
    val assert2 = AssertionResult.pass(())
    val target = TestCase[Unit](for {
      a <- Assertions(
        (assert0 |+| assert1).<::(assert2)
      )
    }yield a)
    assert(target.run(()) == TestResult(()))
  }
}
