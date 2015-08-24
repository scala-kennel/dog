package dog

import sbt.testing._
import scalaz.\/

final case class DogEvent[A](
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long,
  result: Throwable \/ TestResult[A]
) extends Event
