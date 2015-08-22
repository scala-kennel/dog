package dog

import scalaz._

object Assertions {

  @annotation.tailrec
  def apply[A](rs: NonEmptyList[AssertionResult[A]]): TestResult[A] = (rs.head, rs.tail) match {
    case (Passed(v), List()) => TestResult(v)
    case (Passed(_), t::ts) => apply(NonEmptyList.nel(t, ts))
    case (NotPassed(c), ts) => TestResult.nel(NotPassed(c), ts)
  }
}
