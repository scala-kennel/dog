package dog

import scalaprops.{Param => SParam}
import props._

object PropTest extends Dog with Assert {

  implicit val param = SParam.withCurrentTimeSeed()

  val `convert TestCase` = Prop.property((i : Int) => pass(i))
}

