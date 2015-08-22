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
}
