package dog

import scalaz.IList
import scalaprops._

package object props {

  implicit def testcase[A](implicit P: dog.Param): AsProperty[TestCase[A]] =
    AsProperty.from (c => Property.propFromResult(c.run(P) match {
      case Done(results) => results.list match {
        case List(Passed(_)) => Result.Proven
        case List(NotPassed(Skipped(reason))) => Result.Ignored(reason)
        case _ => Result.Falsified(IList.empty)
      }
      case Error(e::_, _) => Result.Exception(IList.empty, e)
      case _ => Result.Falsified(IList.empty)
    }))

  implicit class TestCaseSynax[A](val self: TestCase[A]) {

    def to(param: dog.Param = dog.Param.default): Property = testcase(param).asProperty(self)
  }
}
