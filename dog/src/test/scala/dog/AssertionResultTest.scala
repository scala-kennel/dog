package dog

import scalaz._

object AssertionResultTest extends Dog with Assert {

  val skipAssertionResult = {
    def f: Int = throw new Exception("skip test")
    val expected = TestResult.nel(-\/(Skipped("skip")), IList.empty[AssertionResult[Int]])
    val actual = TestCaseTest.run(pass(f).skip("skip"))
    equal(expected, actual)
  }
}
