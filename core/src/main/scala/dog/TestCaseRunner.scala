package dog

import scalaz._
import ComposableTest._

object DefaultTestCaseRunner extends TestCaseRunner {

  override def apply[A](fa: ComposableTest[A]) = fa match {
    case HandleError(e) => run(TestResult.error(IList.single(e), IList.empty))
    case Assertion(f) => run(AssertionResult.toTestResult(f()))
    case Fixture(f) =>
      f()
      run(TestResult(()))
  }

  private[this] def run[A](result: => TestResult[A]) =
    Kleisli.kleisli((paramEndo: Endo[Param]) => try {
      // TODO: cancellation
      // val p = paramEndo(Param.default)
      lazy val r = result
      r
    } catch {
      case e: Throwable => TestResult.error[A](IList.single(e), IList.empty)
    })
}

object DefaultTestCaseApRunner extends TestCaseApRunner {

  override def apply[A](fa: ComposableTest[A]) = fa match {
    case HandleError(e) => run(ValidationResult.error(IList.single(e), IList.empty))
    case Assertion(f) => run(AssertionResult.toValidationResult(f()))
    case Fixture(f) =>
      f()
      run(ValidationResult(()))
  }

  private[this] def run[A](result: => ValidationResult[A]) =
    Kleisli.kleisli((paramEndo: Endo[Param]) => try {
      // TODO: cancellation
      // val p = paramEndo(Param.default)
      lazy val r = result
      r
    } catch {
      case e: Throwable => ValidationResult.error[A](IList.single(e), IList.empty)
    })
}
