package dog

import scalaz._
import scala.scalajs.js.annotation.JSExportDescendentObjects
import java.util.concurrent.TimeUnit

@JSExportDescendentObjects
trait Dog {

  def paramEndo: Config = Param.id

  def listener: DogListener = DogListener.default

  implicit def testCaseRunner: TestCaseRunner = DefaultTestCaseRunner

  implicit def testCaseApRunner: TestCaseApRunner = DefaultTestCaseApRunner

  implicit class TestCaseParamSyntax[A] private[dog](val self: TestCase[A]) {

    def timeout(n: Int, timeunit: TimeUnit): TestCase[A] =
      self.mapFirstSuspension(Dog.config(Param.timeout(n, timeunit)))
  }

  implicit class TestCaseApParamSyntax[A] private[dog](val self: TestCaseAp[A]) {

    def timeout(n: Int, timeunit: TimeUnit): TestCaseAp[A] =
      self.hoist(Dog.config(Param.timeout(n, timeunit)))
  }
}

private[dog] object Dog {

  def config(other: Config): TestFixture ~> TestFixture = new (TestFixture ~> TestFixture) {
    override def apply[A](fa: TestFixture[A]) =
      LazyTuple2(fa._1 compose other, fa._2)
  }
}
