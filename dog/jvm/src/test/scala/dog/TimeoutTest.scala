package dog

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

object TimeoutTest extends Dog with Assert {

  def isTimeout(target: Throwable): TestCaseAp[Unit] = target match {
    case a: TimeoutException => pass(())
    case _ => fail("expected TimeoutException, but not")
  }

  val `handle timeout` = TestCase {
    val target = TestCase {
      Thread.sleep(100)
      fail[Unit]("should timeout but not")
    }.timeout(1, TimeUnit.MILLISECONDS)
    TestCaseTest.run(target) match {
      case TestResult.Error(scalaz.ICons(e, _), _) => isTimeout(e)
      case _ => fail("should timeout but not")
    }
  }
}
