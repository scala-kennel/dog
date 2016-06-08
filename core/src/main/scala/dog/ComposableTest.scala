package dog

import scalaz._
import scalaz.syntax.apply._
import scala.util.control.NonFatal

sealed abstract class ComposableTest[A] extends Product with Serializable

final case class Fixture(f: () => Unit) extends ComposableTest[Unit]
final case class TestCase[A](result: ValidationResult[A]) extends ComposableTest[A]
final case class Assertion[A](assert: () => AssertionResult[A]) extends ComposableTest[A]

object TestCase {

  def apply[A](test: => TestCasesAp[A]): TestCasesAp[A] =
    (FreeAp.point[ComposableTest, Unit](()) |@| test) { case (_, a) => a }

  def apply[A](test: => TestCases[A]): TestCases[A] =
    Free.point[ComposableTest, Unit](()).flatMap(_ => test)

  def fixture(f: () => Unit): TestCases[Unit] =
    Free.liftF(Fixture(f))
}

trait Assert {

  private[this] def notPassed[A](reason: String): AssertionResult[A] =
    \/.left[NotPassedCause, A](NotPassedCause.violate(reason))

  def equal[A](expected: A, actual: A): TestCasesAp[Unit] =
    FreeAp.lift(Assertion(() =>
      if(expected == actual) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    ))

  def eq[A](expected: A, actual: A)(implicit E: scalaz.Equal[A]): TestCasesAp[Unit] =
    FreeAp.lift(Assertion(() =>
      if(E.equal(expected, actual)) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    ))

  def pass[A](value: A): TestCasesAp[A] = FreeAp.pure(value)

  def fail[A](reason: String): TestCasesAp[A] =
    FreeAp.lift(Assertion(() => notPassed(reason)))

  def pred(p: Boolean): TestCasesAp[Unit] =
    if(p) pass(())
    else fail("fail assertion.")

  def trap[A](f: => A): TestCases[Throwable] = try {
    f
    fail("Expect thrown exn but not").monadic
  } catch {
    case NonFatal(e) => pass(e).monadic
  }
}
