package dog

import sbt.testing._

final case class DogEvent[A](
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long,
  result: TestResult[A]
) extends Event
