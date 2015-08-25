package dog

import scalaprops._
import props._

object Proptest extends Scalaprops {

  val convertTestCase = Property.property((i : Int) => TestCase.ok(i).to)
}

