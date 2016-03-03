package dog

import scala.scalajs.js.annotation.JSExportDescendentObjects
import java.util.concurrent.TimeUnit

@JSExportDescendentObjects
trait Dog {

  def paramEndo: scalaz.Endo[Param] = Param.id

  def listener: DogListener = DogListener.default

  implicit class ParamSyntax[A] private[dog](val self: TestCase[A]) {

    def timeout(n: Int, timeunit: TimeUnit): TestCase[A] =
      self.local(_ compose Param.timeout(n, timeunit))
  }
}
