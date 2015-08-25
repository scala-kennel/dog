package dog

import java.util.concurrent.{TimeUnit, ExecutorService}
import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(
  timeout: Duration = Duration(30, TimeUnit.SECONDS),
  private[dog] executorService: Option[ExecutorService] = None
)

object Param {

  val default: Param = Param()

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  private[dog] def executorService(service: ExecutorService): Endo[Param] =
    Endo(_.copy(executorService = Some(service)))

  val id: Endo[Param] = Endo.idEndo[Param]
}
