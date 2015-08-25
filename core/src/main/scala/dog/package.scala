import scalaz._

package object dog {

  type TestCase[A] = Kleisli[TestResult, Param, A]

  object TestCase {

    def apply[A](result: => TestResult[A]): TestCase[A] = {
      def body(p: Param): TestResult[A] =
        try {
          result
        } catch {
          case e: Throwable => TestResult.error[A](List(e), List())
        }
      Kleisli.kleisli(body)
    }

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
