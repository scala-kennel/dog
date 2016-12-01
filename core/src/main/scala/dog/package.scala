import scalaz._

package object dog {

  type Config = Endo[Param]

  type Test[A] = Kleisli[TestResult, Config, A]

  type TestAp[A] = Kleisli[ValidationResult, Config, A]

  type TestFixture[A] = LazyTuple2[Config, ComposableTest[A]]

  type TestCaseAp[A] = FreeAp[TestFixture, A]

  type TestCase[A] = Free[TestFixture, A]

  type TestCaseRunner = TestFixture ~> Test

  type TestCaseApRunner = TestFixture ~> TestAp

  implicit class TestCaseApSyntax[A](self: => TestCaseAp[A]) {

    def skip(reason: String): TestCaseAp[A] =
      FreeAp.lift[TestFixture, A](LazyTuple2(Param.id, ComposableTest.assertion(() =>
        -\/(NotPassedCause.skip(reason)))))

    def lift: TestCase[A] =
      Free.liftF[TestFixture, A](LazyTuple2(Param.id, ComposableTest.Gosub(() =>
        -\/(self))))
  }

  implicit class TestCaseSyntax[A](self: => TestCase[A]) {

    def skip(reason: String): TestCase[A] =
      Free.liftF[TestFixture, A](LazyTuple2(Param.id, ComposableTest.assertion(() =>
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
