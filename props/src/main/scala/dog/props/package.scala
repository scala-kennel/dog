package dog

import scalaz._
import scalaz.Kleisli._
import scalaprops._
import java.util.concurrent.TimeoutException

package object props {

  implicit def testcase[A](implicit R: TestCaseRunner, E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCases[A]] = {
    import TestResult._
    AsProperty.from (c => Property.propFromResult(c.foldMap(R).run(E) match {
      case Done(results) => results.list match {
        case ICons(\/-(_), INil()) => Result.Proven
        case ICons(-\/(Skipped(reason)), INil()) => Result.Ignored(reason)
        case _ => Result.Falsified(IList.empty)
      }
      case Error(ICons(e, _), _) => Result.Exception(IList.empty, e)
      case _ => Result.Falsified(IList.empty)
    }))
  }

  implicit def testcaseAp[A](implicit R: TestCaseApRunner, E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCasesAp[A]] = {
    import ValidationResult._
    AsProperty.from (c => Property.propFromResult(c.foldMap(R).run(E) match {
      case Done(results) => results.list match {
        case ICons(\/-(_), INil()) => Result.Proven
        case ICons(-\/(Skipped(reason)), INil()) => Result.Ignored(reason)
        case _ => Result.Falsified(IList.empty)
      }
      case Error(ICons(e, _), _) => Result.Exception(IList.empty, e)
      case _ => Result.Falsified(IList.empty)
    }))
  }

  private[this] def checkResultToTestResult(result: CheckResult): ValidationResult[Unit] = result match {
    case _: CheckResult.Proven | _: CheckResult.Passed => ValidationResult(())
    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
      ValidationResult.nel(-\/(NotPassedCause.violate(result.toString)), IList.empty)
    case e: CheckResult.GenException =>
      ValidationResult.error(IList.single(e.exception), IList.empty)
    case e: CheckResult.PropException =>
      ValidationResult.error(IList.single(e.exception), IList.single(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Timeout =>
      ValidationResult.error(IList.empty, IList.single(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Ignored =>
      ValidationResult.nel(-\/(NotPassedCause.skip(e.reason)), IList.empty)
  }

  private[this] def checkProperty(prop: Property, param: scalaprops.Param): ValidationResult[Unit] = {
    var cancel = false
    try {
      // TODO: cancellation
      // val p = paramEndo(dog.Param.default)
      checkResultToTestResult(
        prop.check(param, () => cancel, count => ())
      )
    } catch {
      case e: TimeoutException => ValidationResult.error(IList.single(e), IList.empty)
      case e: Throwable => ValidationResult.error(IList.single(e), IList.empty)
    } finally {
      cancel = true
    }
  }

  implicit class PropertySyntax(val self: Property) {

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCases[Unit] =
      Free.liftF(TestCase(checkProperty(self, param)))
  }

  implicit class PropertiesSyntax[A](val self: Properties[A]) {
    import ValidationResult._

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCases[Unit] =
      Free.liftF(TestCase(
        Tree.treeInstance.foldMap1(self.props.map { case (_, checkOpt) =>
          checkOpt.map(c => checkProperty(c.prop, param))
            .getOrElse(ValidationResult(()))
        })(identity _)
      ))
  }
}
