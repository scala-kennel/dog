package dog

import scalaz._

sealed abstract class AssertionResult[A] {
  def map[B](f: A => B): AssertionResult[B] = this match {
    case Passed(a) => Passed(f(a))
    case NotPassed(c) => NotPassed(c)
  }
}

final case class Passed[A](value: A) extends AssertionResult[A]
final case class NotPassed[A](cause: NotPassedCause) extends AssertionResult[A]

object AssertionResult {

  def apply[A](cause: NotPassedCause): AssertionResult[A] = NotPassed(cause)

  def onlyNotPassed[A](xs: NonEmptyList[AssertionResult[A]]): List[NotPassedCause] =
    xs.list.collect {
      case NotPassed(x) => x
    }

  implicit val functorInstance: Functor[AssertionResult] = new Functor[AssertionResult] {
    override def map[A, B](fa: AssertionResult[A])(f: A => B) = fa.map(f)
  }

  implicit def equalInstance[A](implicit E: Equal[A]): Equal[AssertionResult[A]] = new Equal[AssertionResult[A]] {
    def equal(a1: AssertionResult[A], a2: AssertionResult[A]) = (a1, a2) match {
      case (Passed(v1), Passed(v2)) => E.equal(v1, v2)
      case (NotPassed(c1), NotPassed(c2)) => NotPassedCause.equalInstance.equal(c1, c2)
      case _ => false
    }
  }
}
