package dog

import scalaz._
import scalaprops._
import java.util.concurrent.TimeoutException

package object props {

  implicit def testcase[A](implicit E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCase[A]] =
    AsProperty.from (c => Property.propFromResult(c.run(E) match {
      case Done(results) => results.list match {
        case ICons(\/-(_), INil()) => Result.Proven
        case ICons(-\/(Skipped(reason)), INil()) => Result.Ignored(reason)
        case _ => Result.Falsified(IList.empty)
      }
      case Error(ICons(e, _), _) => Result.Exception(IList.empty, e)
      case _ => Result.Falsified(IList.empty)
    }))

  implicit def assertion[A]: AsProperty[AssertionResult[A]] =
    AsProperty.from(a => Property.propFromResult(a match {
      case \/-(_) => Result.Proven
      case -\/(Skipped(reason)) => Result.Ignored(reason)
      case -\/(Violated(_)) => Result.Falsified(IList.empty)
    }))

  private[this] def checkResultToTestResult(result: CheckResult): TestResult[Unit] = result match {
    case _: CheckResult.Proven | _: CheckResult.Passed => TestResult(())
    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
      TestResult.nel(-\/(NotPassedCause.violate(result.toString)), IList.empty)
    case e: CheckResult.GenException =>
      TestResult.error(IList.single(e.exception), IList.empty)
    case e: CheckResult.PropException =>
      TestResult.error(IList.single(e.exception), IList.single(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Timeout =>
      TestResult.error(IList.empty, IList.single(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Ignored =>
      TestResult.nel(-\/(NotPassedCause.skip(e.reason)), IList.empty)
  }

  private[this] def checkProperty(prop: Property, param: scalaprops.Param, paramEndo: Endo[dog.Param]): TestResult[Unit] = {
    var cancel = false
    try {
      // TODO: cancellation
      // val p = paramEndo(dog.Param.default)
      checkResultToTestResult(
        prop.check(param, () => cancel, count => ())
      )
    } catch {
      case e: TimeoutException => TestResult.error(IList.single(e), IList.empty)
      case e: Throwable => TestResult.error(IList.single(e), IList.empty)
    } finally {
      cancel = true
    }
  }

  implicit class PropertySyntax(val self: Property) {

    def toTestCase(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCase[Unit] =
      Kleisli.kleisli((paramEndo: Endo[dog.Param]) => checkProperty(self, param, paramEndo))
  }

  implicit class PropertiesSyntax[A](val self: Properties[A]) {
    import TestResult._

    def toTestCase(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCase[Unit] =
      Kleisli.kleisli((paramEndo: Endo[dog.Param]) =>
        Tree.treeInstance.foldMap1(self.props.map { case (_, checkOpt) =>
          checkOpt.map(c => checkProperty(c.prop, param, paramEndo))
            .getOrElse(TestResult(()))
        })(identity _)
      )
  }
}
