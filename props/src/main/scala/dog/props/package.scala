package dog

import scalaz._
import scalaz.concurrent.Task
import scalaprops._
import java.util.concurrent.atomic.AtomicBoolean
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

  implicit class TestCaseSynax[A](val self: TestCase[A]) {

    def to(implicit E: Endo[dog.Param] = dog.Param.id): Property = testcase.asProperty(self)
  }

  implicit def assertion[A]: AsProperty[AssertionResult[A]] =
    AsProperty.from(a => Property.propFromResult(a match {
      case \/-(_) => Result.Proven
      case -\/(Skipped(reason)) => Result.Ignored(reason)
      case -\/(Violated(_)) => Result.Falsified(IList.empty)
    }))

  implicit class AssertionResultSynax[A](val self: AssertionResult[A]) {

    def to: Property = assertion.asProperty(self)
  }

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
    val cancel = new AtomicBoolean(false)
    try {
      val p = paramEndo(dog.Param.default)
      checkResultToTestResult((p.executorService match {
        case Some(s) => Task(prop.check(param, cancel, count => ()))(s)
        case None => Task(prop.check(param, cancel, count => ()))
      }).unsafePerformSyncFor(param.timeout))
    } catch {
      case e: TimeoutException => TestResult.error(IList.single(e), IList.empty)
      case e: Throwable => TestResult.error(IList.single(e), IList.empty)
    } finally {
      cancel.set(true)
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
