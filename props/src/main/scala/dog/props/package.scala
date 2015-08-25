package dog

import scalaz._
import scalaprops._

package object props {

  implicit def testcase[A](implicit E: Endo[dog.Param] = dog.Param.id): AsProperty[TestCase[A]] =
    AsProperty.from (c => Property.propFromResult(c.run(E) match {
      case Done(results) => results.list match {
        case List(Passed(_)) => Result.Proven
        case List(NotPassed(Skipped(reason))) => Result.Ignored(reason)
        case _ => Result.Falsified(IList.empty)
      }
      case Error(e::_, _) => Result.Exception(IList.empty, e)
      case _ => Result.Falsified(IList.empty)
    }))

  implicit class TestCaseSynax[A](val self: TestCase[A]) {

    def to(implicit E: Endo[dog.Param] = dog.Param.id): Property = testcase.asProperty(self)
  }

  implicit def assertion[A]: AsProperty[AssertionResult[A]] =
    AsProperty.from(a => Property.propFromResult(a match {
      case Passed(_) => Result.Proven
      case NotPassed(Skipped(reason)) => Result.Ignored(reason)
      case NotPassed(Violated(_)) => Result.Falsified(IList.empty)
    }))

  implicit class AssertionResultSynax[A](val self: AssertionResult[A]) {

    def to: Property = assertion.asProperty(self)
  }
}
