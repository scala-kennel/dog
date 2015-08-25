package dog

import scalaz._
import scalaz.std.list._

sealed abstract class TestResult[A] {

  import TestResult._

  def map[B](f: A => B): TestResult[B] = this match {
    case Done(results) => Done(results.map(_.map(f)))
    case Error(es, cs) => Error(es, cs)
  }

  def flatMap[B](f: A => TestResult[B]): TestResult[B] = this match {
    case Done(results) => results.list match {
      case List(Passed(a)) =>
        try {
          f(a)
        } catch {
          case e: Throwable => error[B](List(e), List())
        }
      case _ => AssertionResult.onlyNotPassed(results) match {
        case List() => throw new Exception("oops!")
        case x::xs => nel(NotPassed(x), xs.map(AssertionResult[B](_)))
      }
    }
    case Error(es, cs) => Error(es, cs)
  }

  def +>(result: AssertionResult[A]): TestResult[A] = this match {
    case Done(results) => results.list match {
      case List(Passed(_)) => result match {
        case Passed(v) => TestResult(v)
        case n@NotPassed(_) => TestResult.nel(n, List())
      }
      case _ => result match {
        case Passed(_) => Done(results)
        case n@NotPassed(_) => TestResult.nel(n, results.list)
      }
    }
    case Error(es, cs) => result match {
      case Passed(_) => error(es, cs)
      case NotPassed(c) => error(es, c :: cs)
    }
  }

  def hasError: Option[Throwable] = this match {
    case Error(e::_, _) => Some(e)
    case _ => None
  }
}


final case class Error[A](exceptions: List[Throwable], causes: List[NotPassedCause]) extends TestResult[A]
final case class Done[A] private[dog] (results: AssertionNel[A]) extends TestResult[A]

object TestResult {

  def apply[A](a: A): TestResult[A] = Done(NonEmptyList.nel(Passed(a), List()))

  def nel[A](n: NotPassed[A], l: List[AssertionResult[A]]): TestResult[A] =
    Done(
      NonEmptyList.nel(
        n.asInstanceOf[AssertionResult[A]],
        l.filter(_ match {
          case NotPassed(_) => true
          case _ => false
        })
      )
    )

  def error[A](es: List[Throwable], cs: List[NotPassedCause]): TestResult[A] =
    Error[A](es, cs)

  implicit val monadInstance: Monad[TestResult] = new Monad[TestResult] {
    override def point[A](a: => A) = TestResult(a)
    override def bind[A, B](fa: TestResult[A])(f: A => TestResult[B]) =
      fa.flatMap(f)
    override def map[A, B](fa: TestResult[A])(f: A => B) = fa.map(f)
  }

  implicit def equalInstance[A: Equal](implicit E: Equal[Throwable]): Equal[TestResult[A]] = new Equal[TestResult[A]] {
    override def equal(a1: TestResult[A], a2: TestResult[A]) = (a1, a2) match {
      case (Done(r1), Done(r2)) =>
        NonEmptyList.nonEmptyListEqual(AssertionResult.equalInstance[A]).equal(r1, r2)
      case (Error(es1, cs1), Error(es2, cs2)) =>
        listEqual(E).equal(es1, es2) && listEqual(NotPassedCause.equalInstance).equal(cs1, cs2)
      case _ => false
    }
  }
}
