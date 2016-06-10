package dog

import scalaz._
import scalaz.Kleisli._
import scalaprops._
import java.util.concurrent.TimeoutException
import ComposableTest._

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

  private[this] def checkResultToComposableTest(result: CheckResult): ComposableTest[Unit] = result match {
    case _: CheckResult.Proven | _: CheckResult.Passed => Assertion(() => \/-(()))
    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
      Assertion(() => -\/(NotPassedCause.violate(result.toString)))
    case e: CheckResult.GenException =>
      HandleError(e.exception)
    case e: CheckResult.PropException =>
      HandleError(e.exception)
    case e: CheckResult.Timeout =>
      Assertion(() => -\/(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Ignored =>
      Assertion(() => -\/(NotPassedCause.skip(e.reason)))
  }

  private[this] def checkProperty(prop: Property, param: scalaprops.Param): ComposableTest[Unit] = {
    var cancel = false
    try {
      // TODO: cancellation
      // val p = paramEndo(dog.Param.default)
      checkResultToComposableTest(
        prop.check(param, () => cancel, count => ())
      )
    } catch {
      case e: TimeoutException => HandleError(e)
      case e: Throwable => HandleError(e)
    } finally {
      cancel = true
    }
  }

  implicit class PropertySyntax(val self: Property) {

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCases[Unit] =
      Free.liftF(checkProperty(self, param))
  }

  implicit class PropertiesSyntax[A](val self: Properties[A]) {
    import scalaz.std.anyVal._
    import scalaz.Free._

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCases[Unit] =
      Tree.treeInstance.foldMap1(self.props.map { case (_, checkOpt) =>
        Free.liftF(
          checkOpt.map(c => checkProperty(c.prop, param))
            .getOrElse(Assertion(() => \/-(())))
        )
      })(identity _)
  }
}
