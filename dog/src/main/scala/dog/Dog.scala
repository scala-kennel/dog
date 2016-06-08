package dog

import scalaz.syntax.apply._
import scala.scalajs.js.annotation.JSExportDescendentObjects
import java.util.concurrent.TimeUnit

@JSExportDescendentObjects
trait Dog {

  def paramEndo: scalaz.Endo[Param] = Param.id

  def listener: DogListener = DogListener.default

  implicit def testCaseRunner: TestCaseRunner = DefaultTestCaseRunner

  implicit def testCaseApRunner: TestCaseApRunner = DefaultTestCaseApRunner

  def Assertion2(test1: TestCasesAp[Unit], test2: TestCasesAp[Unit]): TestCasesAp[Unit] =
    (test1 |@| test2) { case (_, _) => () }

  def Assertion3(test1: TestCasesAp[Unit], test2: TestCasesAp[Unit], test3: TestCasesAp[Unit]): TestCasesAp[Unit] =
    (test1 |@| test2 |@| test3) { case (_, _, _) => () }

  def Assertion4(test1: TestCasesAp[Unit], test2: TestCasesAp[Unit], test3: TestCasesAp[Unit], test4: TestCasesAp[Unit]): TestCasesAp[Unit] =
    (test1 |@| test2 |@| test3 |@| test4) { case (_, _, _, _) => () }
}
