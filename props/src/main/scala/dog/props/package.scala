package dog

import scalaz._
import scalaz.Kleisli._
import scalaprops._
import java.util.concurrent.TimeoutException
import ComposableTest._

package object props {

  implicit def testcase[A](implicit R: TestCaseRunner, E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCase[A]] = {
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

  implicit def testcaseAp[A](implicit R: TestCaseApRunner, E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCaseAp[A]] = {
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
    case _: CheckResult.Proven | _: CheckResult.Passed => assertion(() => \/-(()))
    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
      assertion(() => -\/(NotPassedCause.violate(result.toString)))
    case e: CheckResult.GenException =>
      HandleErrors(IList.single(e.exception))
    case e: CheckResult.PropException =>
      HandleErrors(IList.single(e.exception))
    case e: CheckResult.Timeout =>
      assertion(() => -\/(NotPassedCause.violate(e.toString)))
    case e: CheckResult.Ignored =>
      assertion(() => -\/(NotPassedCause.skip(e.reason)))
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
      case e: TimeoutException => HandleErrors(IList.single(e))
      case e: Throwable => HandleErrors(IList.single(e))
    } finally {
      cancel = true
    }
  }

  implicit class PropertySyntax(val self: Property) {

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCase[Unit] =
      Free.liftF[TestFixture, Unit](LazyTuple2(Param.id, checkProperty(self, param)))
  }

  implicit class PropertiesSyntax[A](val self: Properties[A]) {
    import scalaz.std.anyVal._
    import scalaz.Free._

    def lift(param: scalaprops.Param = scalaprops.Param.withCurrentTimeSeed()): TestCase[Unit] =
      Tree.treeInstance.foldMap1(self.props.map { case (_, checkOpt) =>
        Free.liftF[TestFixture, Unit](LazyTuple2(Param.id, checkOpt.map(c =>
          checkProperty(c.prop, param)).getOrElse(assertion(() => \/-(()))))
        )
      })(identity _)
  }
}
