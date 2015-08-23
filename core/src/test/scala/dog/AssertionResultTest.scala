package dog

import scalaz.std.anyVal._
import scalaprops._
import TestGen._

object AssertionResultTest extends Scalaprops {

  import AssertionResult._

  implicit val gen: Gen[AssertionResult[Int]] = TestGen.assertionResult[Int]

  val laws = scalazlaws.monad.all[AssertionResult]
}
