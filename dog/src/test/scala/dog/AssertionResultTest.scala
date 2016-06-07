package dog

import scalaz.-\/

object AssertionResultTest extends Dog {

  val skipAssertionResult = TestCase {
    def f: Int = throw new Exception("skip test")
    val target = Assert.pass(f).skip("skip")
    Assert.equal(-\/(Skipped("skip")), target)
  }

}
