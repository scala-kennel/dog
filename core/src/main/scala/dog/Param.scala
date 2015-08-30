package dog

import java.util.concurrent.{TimeUnit, ExecutorService}
import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(
  timeout: Duration = Duration(30, TimeUnit.SECONDS),
  executorService: Option[ExecutorService] = None
)

object Param {

  private[dog] val default: Param = Param()

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  def executorService(service: ExecutorService): Endo[Param] =
    Endo(_.copy(executorService = Some(service)))

  val id: Endo[Param] = Endo.idEndo[Param]
}
