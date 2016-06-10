package dog

import scalaz._
import scalaz.Kleisli._
import scalaz.concurrent.Task
import java.util.concurrent.TimeoutException
import ComposableTest._

object DefaultTestCaseRunner extends TestCaseRunner {

  override def apply[A](fa: ComposableTestC[A]) =
    Kleisli.kleisli((paramEndo: Config) => try {
      val config: Config = fa._1.compose(paramEndo)
      val p = config(Param.default)
      (p.executorService match {
        case Some(s) => Task(eval(config, fa))(s)
        case None => Task(eval(config, fa))
      }).unsafePerformSyncFor(p.timeout)
    } catch {
      case e: TimeoutException => TestResult.error(IList.single(e), IList.empty)
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
      (p.executorService match {
        case Some(s) => Task(eval(config, fa))(s)
        case None => Task(eval(config, fa))
      }).unsafePerformSyncFor(p.timeout)
    } catch {
      case e: TimeoutException => ValidationResult.error(IList.single(e), IList.empty)
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
