import scalaz._

package object dog {

  type RunnableTestCase[A] = Kleisli[TestResult, Endo[Param], A]

  type RunnableTestCaseV[A] = Kleisli[ValidationResult, Endo[Param], A]

  type TestCasesAp[A] = FreeAp[ComposableTest, A]

  type TestCases[A] = Free[ComposableTest, A]

  type TestCaseRunner = ComposableTest ~> RunnableTestCase

  type TestCaseApRunner = ComposableTest ~> RunnableTestCaseV

  implicit class TestCasesApSyntax[A](self: => TestCasesAp[A]) {

    def skip(reason: String): TestCasesAp[A] =
      FreeAp.lift(Assertion(() => -\/(NotPassedCause.skip(reason))))
  }

  type AssertionResult[A] = NotPassedCause \/ A

  type AssertionNel[A] = NonEmptyList[AssertionResult[A]]

  object AssertionResult {

    def onlyNotPassed[A](xs: AssertionNel[A]): IList[NotPassedCause] =
      xs.list.collect { case -\/(x) => x }

    val toTestResult = new (AssertionResult ~> TestResult) {
      def apply[A](r: AssertionResult[A]): TestResult[A] = r match {
        case \/-(v) => TestResult(v)
        case -\/(c) => TestResult.nel(-\/(c), IList.empty)
      }
    }

    val toValidationResult = new (AssertionResult ~> ValidationResult) {
      def apply[A](r: AssertionResult[A]): ValidationResult[A] = r match {
        case \/-(v) => ValidationResult(v)
        case -\/(c) => ValidationResult.nel(-\/(c), IList.empty)
      }
    }
  }
}
