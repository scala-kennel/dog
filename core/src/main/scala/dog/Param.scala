package dog

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(timeout: Duration = Duration(30, TimeUnit.SECONDS))

object Param {

  val default: Param = Param()

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  val id: Endo[Param] = Endo.idEndo[Param]
}
