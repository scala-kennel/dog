package dog

import scalaz._
import scalaz.syntax.apply._
import scala.util.control.NonFatal

sealed abstract class ComposableTest[A] extends Product with Serializable

object ComposableTest {

  final case class Fixture(f: () => Unit) extends ComposableTest[Unit]
  final case class Gosub[A](f: () => TestCaseAp[A] \/ TestCase[A]) extends ComposableTest[A]
  final case class HandleErrors[A](es: IList[Throwable]) extends ComposableTest[A]
  final case class Assertion[A](assert: () => AssertionResult[A]) extends ComposableTest[A]

  def assertion[A](f: () => AssertionResult[A]): ComposableTest[A] =
    Assertion(f)

  def handle[A](es: IList[Throwable]): ComposableTest[A] =
    HandleErrors(es)

  def fixture(f: () => Unit): ComposableTest[Unit] =
    Fixture(f)
}

object TestCase {

  def apply[A](test: => TestCaseAp[A]): TestCaseAp[A] = {
    lazy val t = test
    FreeAp.lift[TestFixture, A](LazyTuple2(Param.id, ComposableTest.Gosub(() => -\/(t))))
  }

  def apply[A](test: => TestCase[A]): TestCase[A] = {
    lazy val t = test
    Free.liftF[TestFixture, A](LazyTuple2(Param.id, ComposableTest.Gosub(() => \/-(t))))
  }

  def handle[A](es: IList[Throwable]): TestCaseAp[A] =
    FreeAp.lift[TestFixture, A](LazyTuple2(Param.id, ComposableTest.HandleErrors(es)))

  def fixture(f: () => Unit): TestCase[Unit] =
    Free.liftF[TestFixture, Unit](LazyTuple2(Param.id, ComposableTest.fixture(f)))
}

trait Assert { self =>

  import ComposableTest._

  def assert: Assert = self

  private[this] def notPassed[A](reason: String): AssertionResult[A] =
    \/.left[NotPassedCause, A](NotPassedCause.violate(reason))

  def equal[A](expected: A, actual: A): TestCaseAp[Unit] =
    FreeAp.lift[TestFixture, Unit](LazyTuple2(Param.id, assertion(() =>
      if(expected == actual) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    )))

  def eq[A](expected: A, actual: A)(implicit E: scalaz.Equal[A]): TestCaseAp[Unit] =
    FreeAp.lift[TestFixture, Unit](LazyTuple2(Param.id, assertion(() =>
      if(E.equal(expected, actual)) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    )))

  def pass[A](value: A): TestCaseAp[A] = FreeAp.pure(value)

  def fail[A](reason: String): TestCaseAp[A] =
    FreeAp.lift[TestFixture, A](LazyTuple2(Param.id, assertion(() => notPassed(reason))))

  def pred(p: Boolean): TestCaseAp[Unit] =
    if(p) pass(())
    else fail("fail assertion.")

  def apply(p: Boolean): TestCaseAp[Unit] = pred(p)

  def trap[A](f: => A): TestCase[Throwable] = try {
    f
    fail("Expect thrown exn but not").monadic
  } catch {
    case NonFatal(e) => pass(e).monadic
  }

  implicit class TestCaseApUnitSyntax private[dog](val test: TestCaseAp[Unit]) {

    private[this] def compose(other: TestCaseAp[Unit]) =
      (test |@| other) { case (_, _) => () }

    def equal[A](expected: A, actual: A) =
      compose(self.equal(expected, actual))

    def eq[A](expected: A, actual: A)(implicit E: scalaz.Equal[A]) =
      compose(self.eq(expected, actual))

    def pass[A](value: A) = test.map(_ => value)

    def fail[A](reason: String) =
      (test |@| self.fail[A](reason)) { case (_, v) => v }

    def pred(p: Boolean) = compose(self.pred(p))
  }
}
