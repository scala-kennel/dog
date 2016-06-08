package dog

import scalaz._

sealed abstract class TestResult[A] {

  import TestResult._

  def map[B](f: A => B): TestResult[B] = this match {
    case Done(results) => Done(results.map(_.map(f)))
    case Error(es, cs) => Error(es, cs)
  }

  def flatMap[B](f: A => TestResult[B]): TestResult[B] = this match {
    case Done(results) => results.list match {
      case ICons(\/-(a), INil()) =>
        try {
          f(a)
        } catch {
          case e: Throwable => error[B](IList.single(e), IList.empty)
        }
      case _ => AssertionResult.onlyNotPassed(results) match {
        case INil() => throw new Exception("oops!")
        case ICons(x, xs) => nel(-\/(x), xs.map(\/.left[NotPassedCause, B](_)))
      }
    }
    case Error(es, cs) => Error(es, cs)
  }

  def hasError: Option[Throwable] = this match {
    case Error(ICons(e, _), _) => Some(e)
    case _ => None
  }

  def append(result: TestResult[A]): TestResult[A] = this match {
    case Done(results) => results match {
      case NonEmptyList(\/-(_), INil()) => result
      case NonEmptyList((r @ -\/(_)), rss) => result match {
        case Done(rs2) => rs2.list match {
          case ICons(\/-(_), INil()) => this
          case _ => TestResult.nel(r, (rss ++ AssertionResult.onlyNotPassed(rs2).map(-\/(_))))
        }
        case Error(es, cs) => error(es, AssertionResult.onlyNotPassed(results) ++ cs)
      }
    }
    case Error(es1, cs1) => result match {
      case Done(results) => error(es1, cs1 ++ AssertionResult.onlyNotPassed(results))
      case Error(es2, cs2) => error(es1 ++ es2, cs1 ++ cs2)
    }
  }
}

object TestResult {

  final case class Error[A](exceptions: IList[Throwable], causes: IList[NotPassedCause]) extends TestResult[A]
  final case class Done[A] private[dog] (results: AssertionNel[A]) extends TestResult[A]

  def apply[A](a: A): TestResult[A] =
    Done(NonEmptyList.nel(\/-(a), IList.empty[AssertionResult[A]]))

  def nel[A](n: -\/[NotPassedCause], as: IList[AssertionResult[A]]): TestResult[A] =
    Done(NonEmptyList.nel(n.asInstanceOf[AssertionResult[A]], as.filter(_.isLeft)))

  def error[A](es: IList[Throwable], cs: IList[NotPassedCause]): TestResult[A] =
    Error[A](es, cs)

  implicit val monadInstance: Monad[TestResult] = new Monad[TestResult] {
    override def point[A](a: => A) = TestResult(a)
    override def bind[A, B](fa: TestResult[A])(f: A => TestResult[B]) =
      fa.flatMap(f)
    override def map[A, B](fa: TestResult[A])(f: A => B) = fa.map(f)
  }

  implicit def semigroupInstance[A]: Semigroup[TestResult[A]] = new Semigroup[TestResult[A]] {
    override def append(f1: TestResult[A], f2: => TestResult[A]) = f1.append(f2)
  }

  implicit def equalInstance[A: Equal](implicit E: Equal[Throwable]): Equal[TestResult[A]] = new Equal[TestResult[A]] {
    import NotPassedCause._
    override def equal(a1: TestResult[A], a2: TestResult[A]) = (a1, a2) match {
      case (Done(r1), Done(r2)) =>
        NonEmptyList.nonEmptyListEqual(\/.DisjunctionEqual[NotPassedCause, A]).equal(r1, r2)
      case (Error(es1, cs1), Error(es2, cs2)) =>
        IList.equal(E).equal(es1, es2) && IList.equal[NotPassedCause].equal(cs1, cs2)
      case _ => false
    }
  }
}
