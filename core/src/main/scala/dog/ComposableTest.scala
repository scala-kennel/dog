package dog

import scalaz._

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
