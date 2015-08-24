import scalaz._

package object dog {

  type TestCase[A] = Kleisli[TestResult, Unit, A]

  object TestCase {

    def apply[A](result : => TestResult[A]): TestCase[A] = {
      def body(a: Unit): TestResult[A] =
        try {
          result
        } catch {
          case e: Throwable => TestResult.error[A](List(e), List())
        }
      Kleisli.kleisli(body)
    }
  }

  type AssertionNel[A] = NonEmptyList[AssertionResult[A]]

  implicit def toTestResult[A](result: AssertionResult[A]): TestResult[A] =
    AssertionResult.toTestResult(result)
}
