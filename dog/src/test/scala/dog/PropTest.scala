package dog

import scalaprops._
import props._

object PropTest extends Dog {

  val convertTestCase = Property.property((i : Int) => TestCase.ok(i).to)
    .toTestCase()

  val convertAssertionResult = Property.property((i : Int) => Assert.pass(()).to)
    .toTestCase()
}

