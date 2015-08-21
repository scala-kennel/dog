package dog

import scalaz._
import scalaz.std.string.stringInstance

sealed abstract class NotPassedCause
final case class Skipped(reason: String) extends NotPassedCause
final case class Violated(reason: String) extends NotPassedCause

object NotPassedCause {

  def equalInstance: Equal[NotPassedCause] = new Equal[NotPassedCause] {
    override def equal(a1: NotPassedCause, a2: NotPassedCause) = (a1, a2) match {
      case (Violated(r1), Violated(r2)) => stringInstance.equal(r1, r2)
      case (Skipped(r1), Skipped(r2)) => stringInstance.equal(r1, r2)
      case _ => false
    }
  }
}
