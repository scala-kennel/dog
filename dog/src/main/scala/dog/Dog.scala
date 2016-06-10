package dog

import scalaz._
import scalaz.syntax.apply._
import scala.scalajs.js.annotation.JSExportDescendentObjects
import java.util.concurrent.TimeUnit

@JSExportDescendentObjects
trait Dog {

  def paramEndo: Config = Param.id

  def listener: DogListener = DogListener.default

  implicit def testCaseRunner: TestCaseRunner = DefaultTestCaseRunner

  implicit def testCaseApRunner: TestCaseApRunner = DefaultTestCaseApRunner

  def Assertion2(test1: TestCaseAp[Unit], test2: TestCaseAp[Unit]): TestCaseAp[Unit] =
    (test1 |@| test2) { case (_, _) => () }

  def Assertion3(test1: TestCaseAp[Unit], test2: TestCaseAp[Unit], test3: TestCaseAp[Unit]): TestCaseAp[Unit] =
    (test1 |@| test2 |@| test3) { case (_, _, _) => () }

  def Assertion4(test1: TestCaseAp[Unit], test2: TestCaseAp[Unit], test3: TestCaseAp[Unit], test4: TestCaseAp[Unit]): TestCaseAp[Unit] =
    (test1 |@| test2 |@| test3 |@| test4) { case (_, _, _, _) => () }

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

  def config(other: Config): ComposableTestC ~> ComposableTestC = new (ComposableTestC ~> ComposableTestC) {
    override def apply[A](fa: ComposableTestC[A]) =
      LazyTuple2(fa._1 compose other, fa._2)
  }
}
