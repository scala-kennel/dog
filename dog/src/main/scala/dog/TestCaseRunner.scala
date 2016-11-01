package dog

import scalaz._
import scalaz.Kleisli._
import ComposableTest._

object DefaultTestCaseRunner extends TestCaseRunner {

  override def apply[A](fa: ComposableTestC[A]) =
    Kleisli.kleisli((paramEndo: Config) => try {
      val config: Config = fa._1.compose(paramEndo)
      val p = config(Param.default)
      p.executor.execute(p.timeout)(eval(config, fa))
    } catch {
      case e: Throwable => TestResult.error[A](IList.single(e), IList.empty)
    })

  private[this] def eval[A](config: Config, fa: => ComposableTestC[A]): TestResult[A] = fa._2 match {
    case HandleError(e) => TestResult.error(IList.single(e), IList.empty)
    case Assertion(f) => AssertionResult.toTestResult(f())
    case Fixture(f) =>
      f()
      TestResult(())
    case Gosub(f) =>
      f().fold(
        _.foldMap(DefaultTestCaseApRunner).run(config).toTestResult,
        _.foldMap(DefaultTestCaseRunner).run(config)
      )
  }
}

object DefaultTestCaseApRunner extends TestCaseApRunner {

  override def apply[A](fa: ComposableTestC[A]) =
    Kleisli.kleisli((paramEndo: Config) => try {
      val config: Config = fa._1.compose(paramEndo)
      val p = config(Param.default)
      p.executor.execute(p.timeout)(eval(config, fa))
    } catch {
      case e: Throwable => ValidationResult.error[A](IList.single(e), IList.empty)
    })

  private[this] def eval[A](config: Config, fa: => ComposableTestC[A]): ValidationResult[A] = fa._2 match {
    case HandleError(e) => ValidationResult.error(IList.single(e), IList.empty)
    case Assertion(f) => AssertionResult.toValidationResult(f())
    case Fixture(f) =>
      f()
      ValidationResult(())
    case Gosub(f) =>
      f().fold(
        _.foldMap(DefaultTestCaseApRunner).run(config),
        _.foldMap(DefaultTestCaseRunner).run(config).toValidationResult
      )
  }
}
