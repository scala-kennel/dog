import scalaz._

package object dog {

  type TestCase[A] = Kleisli[TestResult, Endo[Param], A]

  implicit def testResultBridge: Bridge[TestResult] = new Bridge[TestResult] {
    override def toTestCase[A](value: => TestResult[A]) =
      Kleisli.kleisli((paramEndo: Endo[Param]) => try {
        // TODO: cancellation
        // val p = paramEndo(Param.default)
        lazy val r = value
        r
      } catch {
        case e: Throwable => TestResult.error[A](IList.single(e), IList.empty)
      })
  }

  implicit def testCaseBridge: Bridge[TestCase] = new Bridge[TestCase] {
    override def toTestCase[A](value: => TestCase[A]) = value
  }

  implicit def assertionResultBridge: Bridge[AssertionResult] = new Bridge[AssertionResult] {
    override def toTestCase[A](value: => AssertionResult[A]) = {
      lazy val v = value
      testResultBridge.toTestCase(AssertionResult.toTestResult(v))
    }
  }

  object TestCase {

    def apply[F[_], A](value: => F[A])(implicit B: Bridge[F]): TestCase[A] = B.toTestCase(value)

    def ok[A](value: A): TestCase[A] = apply(TestResult(value))

    def fixture(f: () => Unit): TestCase[Unit] = {
      f()
      ok(())
    }
  }

  implicit class TestCaseSyntax[A](self: TestCase[A]) {

    def +>(result: AssertionResult[A]): TestCase[A] = self.mapK(_ +> result)
  }

  implicit class TestCaseByNameSyntax[A](self: => TestCase[A]) {

    def skip(reason: String): TestCase[A] = testResultBridge.toTestCase(
      Done(NonEmptyList.nel(-\/(NotPassedCause.skip(reason)), IList.empty))
    )
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
  }

  implicit class AssertionResultSyntax[A](self: AssertionResult[A]) {

    def +>(result: => AssertionResult[A]): TestCase[A] = testResultBridge.toTestCase((self, result) match {
      case (\/-(_), \/-(v)) => TestResult(v)
      case (\/-(_), p @ -\/(_)) => TestResult.nel(p, IList.empty)
      case (p @ -\/(_), \/-(_)) => TestResult.nel(p, IList.empty)
      case (p1 @ -\/(_), p2 @ -\/(_)) => TestResult.nel(p1, IList.single(p2))
    })

    def toTestCase: TestCase[A] = assertionResultBridge.toTestCase(self)
  }

  implicit class AssertionResultByNameSyntax[A](self: => AssertionResult[A]) {

    def skip(reason: String): AssertionResult[A] = -\/(NotPassedCause.skip(reason))
  }

}
