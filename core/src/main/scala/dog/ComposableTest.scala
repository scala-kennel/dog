package dog

import scalaz._
import scala.util.control.NonFatal

sealed abstract class ComposableTest[A] extends Product with Serializable

object ComposableTest {

  final case class Fixture(f: () => Unit) extends ComposableTest[Unit]
  final case class Gosub[A](f: () => TestCaseAp[A] \/ TestCase[A]) extends ComposableTest[A]
  final case class HandleError[A](e: Throwable) extends ComposableTest[A]
  final case class Assertion[A](assert: () => AssertionResult[A]) extends ComposableTest[A]

  def assertion[A](f: () => AssertionResult[A]): ComposableTest[A] =
    Assertion(f)

  def fixture(f: () => Unit): ComposableTest[Unit] =
    Fixture(f)
}

object TestCase {

  def apply[A](test: => TestCaseAp[A]): TestCaseAp[A] = {
    lazy val t = test
    FreeAp.lift[ComposableTestC, A](LazyTuple2(Param.id, ComposableTest.Gosub(() => -\/(t))))
  }

  def apply[A](test: => TestCase[A]): TestCase[A] = {
    lazy val t = test
    Free.liftF[ComposableTestC, A](LazyTuple2(Param.id, ComposableTest.Gosub(() => \/-(t))))
  }

  def fixture(f: () => Unit): TestCase[Unit] =
    Free.liftF[ComposableTestC, Unit](LazyTuple2(Param.id, ComposableTest.fixture(f)))
}

trait Assert {

  import ComposableTest._

  private[this] def notPassed[A](reason: String): AssertionResult[A] =
    \/.left[NotPassedCause, A](NotPassedCause.violate(reason))

  def equal[A](expected: A, actual: A): TestCaseAp[Unit] =
    FreeAp.lift[ComposableTestC, Unit](LazyTuple2(Param.id, assertion(() =>
      if(expected == actual) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    )))

  def eq[A](expected: A, actual: A)(implicit E: scalaz.Equal[A]): TestCaseAp[Unit] =
    FreeAp.lift[ComposableTestC, Unit](LazyTuple2(Param.id, assertion(() =>
      if(E.equal(expected, actual)) \/-(())
      else notPassed(s"expected: ${expected.toString}, but was: ${actual.toString}")
    )))

  def pass[A](value: A): TestCaseAp[A] = FreeAp.pure(value)

  def fail[A](reason: String): TestCaseAp[A] =
    FreeAp.lift[ComposableTestC, A](LazyTuple2(Param.id, assertion(() => notPassed(reason))))

  def pred(p: Boolean): TestCaseAp[Unit] =
    if(p) pass(())
    else fail("fail assertion.")

  def trap[A](f: => A): TestCase[Throwable] = try {
    f
    fail("Expect thrown exn but not").monadic
  } catch {
    case NonFatal(e) => pass(e).monadic
  }
}
