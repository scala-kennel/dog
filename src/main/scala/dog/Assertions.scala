package dog

import scalaz._

object Assertions {

  val toTestResult = new (AssertionNel ~> TestResult) {
    @annotation.tailrec
    def apply[A](rs: AssertionNel[A]): TestResult[A] = (rs.head, rs.tail) match {
      case (Passed(v), List()) => TestResult(v)
      case (Passed(_), t::ts) => apply(NonEmptyList.nel(t, ts))
      case (NotPassed(c), ts) => TestResult.nel(NotPassed(c), ts)
    }
  }

  def apply[A](rs: AssertionNel[A]): TestResult[A] =
    toTestResult(rs)

  def apply[A](r: AssertionResult[A]): TestResult[A] =
    AssertionResult.toTestResult(r)
}
