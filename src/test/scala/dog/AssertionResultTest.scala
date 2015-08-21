package dog

import scalaz.std.anyVal._
import scalaprops._

object AssertionResultTest extends Scalaprops {

  import AssertionResult._

  implicit val gen: Gen[AssertionResult[Int]] = TestGen.assertionResult[Int]

  val laws = scalazlaws.functor.all[AssertionResult]
}
