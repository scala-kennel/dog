package dog

import scalaz._
import ComposableTest._

object DefaultTestCaseRunner extends TestCaseRunner {

  override def apply[A](fa: ComposableTestC[A]) =
    Kleisli.kleisli((paramEndo: Config) => try {
      // TODO: cancellation
      // val p = fa._1.compose(paramEndo)(Param.default)
      lazy val r: TestResult[A] = fa._2 match {
        case HandleError(e) => TestResult.error(IList.single(e), IList.empty)
        case Assertion(f) => AssertionResult.toTestResult(f())
        case Fixture(f) =>
          f()
          TestResult(())
        case Gosub(f) =>
          f().fold(
            _.foldMap(DefaultTestCaseApRunner).run(paramEndo).toTestResult,
            _.foldMap(DefaultTestCaseRunner).run(paramEndo)
          )
      }
      r
    } catch {
      case e: Throwable => TestResult.error[A](IList.single(e), IList.empty)
    })
}

object DefaultTestCaseApRunner extends TestCaseApRunner {

  override def apply[A](fa: ComposableTestC[A]) =
    Kleisli.kleisli((paramEndo: Endo[Param]) => try {
      // TODO: cancellation
      // val p = fa._1.compose(paramEndo)(Param.default)
      lazy val r: ValidationResult[A] = fa._2 match {
        case HandleError(e) => ValidationResult.error(IList.single(e), IList.empty)
        case Assertion(f) => AssertionResult.toValidationResult(f())
        case Fixture(f) =>
          f()
          ValidationResult(())
        case Gosub(f) =>
          f().fold(
            _.foldMap(DefaultTestCaseApRunner).run(paramEndo),
            _.foldMap(DefaultTestCaseRunner).run(paramEndo).toValidationResult
          )
      }
      r
    } catch {
      case e: Throwable => ValidationResult.error[A](IList.single(e), IList.empty)
    })
}
