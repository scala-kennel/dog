import scalaz._
import scalaz.concurrent.Task
import java.util.concurrent.TimeoutException

package object dog {

  type TestCase[A] = Kleisli[TestResult, Endo[Param], A]

  object TestCase {

    def apply[A](result: => TestResult[A]): TestCase[A] =
      Kleisli.kleisli((paramEndo: Endo[Param]) => try {
        val p = paramEndo(Param.default)
        (p.executorService match {
          case Some(s) => Task(result)(s)
          case None => Task(result)
        }).runFor(p.timeout)
      } catch {
        case e: TimeoutException => TestResult.error[A](List(e), List())
        case e: Throwable => TestResult.error[A](List(e), List())
      })

    def ok[A](value: A): TestCase[A] = apply(TestResult(value))
  }

  type AssertionNel[A] = NonEmptyList[AssertionResult[A]]

  implicit def toTestCase[A](result: AssertionResult[A]): TestCase[A] =
    TestCase(AssertionResult.toTestResult(result))

  implicit class TestCaseSyntax[A] private[dog](val self: TestCase[A]) {

    def skip(reason: String): TestCase[A] = TestCase(
      Done(NonEmptyList.nel(AssertionResult[A](NotPassedCause.skip(reason)), List()))
    )

    def +>(result: AssertionResult[A]): TestCase[A] = self.mapK(_ +> result)
  }
}
