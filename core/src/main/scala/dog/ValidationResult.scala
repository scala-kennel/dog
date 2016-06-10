package dog

import scalaz._

sealed abstract class ValidationResult[A] {

  import ValidationResult._

  def map[B](f: A => B): ValidationResult[B] = this match {
    case Done(results) => Done(results.map(_.map(f)))
    case Error(es, cs) => Error(es, cs)
  }

  def ap[B](x: => ValidationResult[A => B]): ValidationResult[B] = (this, x) match {
    case (Done(as), Done(fs)) => (as.list, fs.list) match {
      case (ICons(\/-(a), INil()), ICons(\/-(f), INil())) =>
        try {
          apply(f(a))
        } catch {
          case e: Throwable => error[B](IList.single(e), IList.empty)
        }
      case (_, ICons(\/-(_), INil())) => AssertionResult.onlyNotPassed(as) match {
        case INil() => throw new Exception("oops!")
        case ICons(x, xs) => nel(-\/(x), xs.map(\/.left[NotPassedCause, B](_)))
      }
      case (ICons(\/-(_), INil()), _) => AssertionResult.onlyNotPassed(fs) match {
        case INil() => throw new Exception("oops!")
        case ICons(x, xs) => nel(-\/(x), xs.map(\/.left[NotPassedCause, B](_)))
      }
      case _ => AssertionResult.onlyNotPassed(as) ++ AssertionResult.onlyNotPassed(fs) match {
        case INil() => throw new Exception("oops!")
        case ICons(x, xs) => nel(-\/(x), xs.map(\/.left[NotPassedCause, B](_)))
      }
    }
    case (Error(es, cs), Done(_)) => Error(es, cs)
    case (Done(_), Error(es, cs)) => Error(es, cs)
    case (Error(es1, cs1), Error(es2, cs2)) => Error(es1 ++ es2, cs1 ++ cs2)
  }

  def hasError: Option[Throwable] = this match {
    case Error(ICons(e, _), _) => Some(e)
    case _ => None
  }

  def append(result: ValidationResult[A]): ValidationResult[A] = this match {
    case Done(results) => results match {
      case NonEmptyList(\/-(_), INil()) => result
      case NonEmptyList((r @ -\/(_)), rss) => result match {
        case Done(rs2) => rs2.list match {
          case ICons(\/-(_), INil()) => this
          case _ => ValidationResult.nel(r, (rss ++ AssertionResult.onlyNotPassed(rs2).map(-\/(_))))
        }
        case Error(es, cs) => error(es, AssertionResult.onlyNotPassed(results) ++ cs)
      }
    }
    case Error(es1, cs1) => result match {
      case Done(results) => error(es1, cs1 ++ AssertionResult.onlyNotPassed(results))
      case Error(es2, cs2) => error(es1 ++ es2, cs1 ++ cs2)
    }
  }

  private[dog] def toTestResult: TestResult[A] = this match {
    case Done(results) => TestResult.Done(results)
    case Error(es, cs) => TestResult.Error(es, cs)
  }
}

object ValidationResult {

  final case class Error[A](exceptions: IList[Throwable], causes: IList[NotPassedCause]) extends ValidationResult[A]
  final case class Done[A] private[dog] (results: AssertionNel[A]) extends ValidationResult[A]

  def apply[A](a: A): ValidationResult[A] =
    Done(NonEmptyList.nel(\/-(a), IList.empty[AssertionResult[A]]))

  def nel[A](n: -\/[NotPassedCause], as: IList[AssertionResult[A]]): ValidationResult[A] =
    Done(NonEmptyList.nel(n.asInstanceOf[AssertionResult[A]], as.filter(_.isLeft)))

  def error[A](es: IList[Throwable], cs: IList[NotPassedCause]): ValidationResult[A] =
    Error[A](es, cs)

  implicit val applicativeInstance: Applicative[ValidationResult] = new Applicative[ValidationResult] {
    override def point[A](a: => A) = ValidationResult(a)
    override def ap[A, B](fa: => ValidationResult[A])(f: => ValidationResult[A => B]) =
      fa.ap(f)
    override def map[A, B](fa: ValidationResult[A])(f: A => B) = fa.map(f)
  }

  implicit def semigroupInstance[A]: Semigroup[ValidationResult[A]] = new Semigroup[ValidationResult[A]] {
    override def append(f1: ValidationResult[A], f2: => ValidationResult[A]) = f1.append(f2)
  }

  implicit def equalInstance[A: Equal](implicit E: Equal[Throwable]): Equal[ValidationResult[A]] = new Equal[ValidationResult[A]] {
    import NotPassedCause._
    override def equal(a1: ValidationResult[A], a2: ValidationResult[A]) = (a1, a2) match {
      case (Done(r1), Done(r2)) =>
        NonEmptyList.nonEmptyListEqual(\/.DisjunctionEqual[NotPassedCause, A]).equal(r1, r2)
      case (Error(es1, cs1), Error(es2, cs2)) =>
        IList.equal(E).equal(es1, es2) && IList.equal[NotPassedCause].equal(cs1, cs2)
      case _ => false
    }
  }
}
