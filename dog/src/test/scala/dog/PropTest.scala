package dog

import scalaprops.{Param => SParam}
import props._

object PropTest extends Dog {

  implicit val param = SParam.withCurrentTimeSeed()

  val convertTestCase = Prop.property((i : Int) => TestCase.ok(i).to)

  val convertAssertionResult = Prop.property((i : Int) => Assert.pass(()).to)
}

