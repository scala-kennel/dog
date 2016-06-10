import scalaz._

package object dog {

  type Config = Endo[Param]

  type RunnableTestCase[A] = Kleisli[TestResult, Config, A]

  type RunnableTestCaseV[A] = Kleisli[ValidationResult, Config, A]

  type ComposableTestC[A] = LazyTuple2[Config, ComposableTest[A]]

  type TestCaseAp[A] = FreeAp[ComposableTestC, A]

  type TestCase[A] = Free[ComposableTestC, A]

  type TestCaseRunner = ComposableTestC ~> RunnableTestCase

  type TestCaseApRunner = ComposableTestC ~> RunnableTestCaseV

  implicit class TestCaseApSyntax[A](self: => TestCaseAp[A]) {

    def skip(reason: String): TestCaseAp[A] =
      FreeAp.lift[ComposableTestC, A](LazyTuple2(Param.id, ComposableTest.assertion(() =>
        -\/(NotPassedCause.skip(reason)))))
  }

  type AssertionResult[A] = NotPassedCause \/ A

  type AssertionNel[A] = NonEmptyList[AssertionResult[A]]

  object AssertionResult {

    def onlyNotPassed[A](xs: AssertionNel[A]): IList[NotPassedCause] =
      xs.list.collect { case -\/(x) => x }

    val toTestResult: AssertionResult ~> TestResult = new (AssertionResult ~> TestResult) {
      def apply[A](r: AssertionResult[A]): TestResult[A] = r match {
        case \/-(v) => TestResult(v)
        case -\/(c) => TestResult.nel(-\/(c), IList.empty)
      }
    }

    val toValidationResult: AssertionResult ~> ValidationResult = new (AssertionResult ~> ValidationResult) {
      def apply[A](r: AssertionResult[A]): ValidationResult[A] = r match {
        case \/-(v) => ValidationResult(v)
        case -\/(c) => ValidationResult.nel(-\/(c), IList.empty)
      }
    }
  }
}
