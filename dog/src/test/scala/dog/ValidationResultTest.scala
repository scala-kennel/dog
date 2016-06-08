package dog

import scalaz._
import scalaz.std.anyVal._
import scalaprops._
import TestGen._
import props._

object ValidationResultTest extends Dog with Assert {

  import ValidationResult._

  implicit val exnEqual: Equal[Throwable] = new Equal[Throwable] {
    override def equal(a1: Throwable, a2: Throwable) = a1 == a2
  }

  implicit val exn: Gen[Throwable] = Gen.value(new Exception())

  implicit val gen: Gen[ValidationResult[Int]] = TestGen.validationResult[Int]

  val laws = Properties.list(
    scalazlaws.applicative.all[ValidationResult],
    scalazlaws.semigroup.all[ValidationResult[Int]],
    scalazlaws.equal.all[ValidationResult[Int]]
  ).lift()
}
