package dog

import scalaz._

sealed abstract class AssertionResult[A] {

  import AssertionResult._

  def map[B](f: A => B): AssertionResult[B] = this match {
    case Passed(a) => pass(f(a))
    case NotPassed(c) => apply(c)
  }

  def flatMap[B](f: A => AssertionResult[B]): AssertionResult[B] = this match {
    case Passed(a) => f(a)
    case NotPassed(c) => apply(c)
  }

  def |+|(a: => AssertionResult[A]): NonEmptyList[AssertionResult[A]] = (this, a) match {
    case (Passed(_), p@Passed(_)) => NonEmptyList.nel(p, List())
    case (Passed(_), p@NotPassed(_)) => NonEmptyList.nel(p, List())
    case (p@NotPassed(_), Passed(_)) => NonEmptyList.nel(p, List())
    case (p1@NotPassed(_), p2@NotPassed(_)) => NonEmptyList.nel(p1, List(p2))
  }
}

final case class Passed[A](value: A) extends AssertionResult[A]
final case class NotPassed[A](cause: NotPassedCause) extends AssertionResult[A]

object AssertionResult {

  def apply[A](cause: NotPassedCause): AssertionResult[A] = NotPassed(cause)

  def pass[A](value: A): AssertionResult[A] = Passed(value)

  def onlyNotPassed[A](xs: NonEmptyList[AssertionResult[A]]): List[NotPassedCause] =
    xs.list.collect {
      case NotPassed(x) => x
    }

  implicit val monadInstance: Monad[AssertionResult] = new Monad[AssertionResult] {
    override def point[A](a: => A) = pass(a)
    override def bind[A, B](fa: AssertionResult[A])(f: A => AssertionResult[B]) =
      fa.flatMap(f)
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
