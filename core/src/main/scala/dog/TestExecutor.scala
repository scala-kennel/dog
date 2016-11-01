package dog

import scala.concurrent.duration.Duration

abstract class TestExecutor {
  def execute[A](timeout: Duration)(f: => A): A
  def shutdown(): Unit
}

private[dog] object TestExecutor {

  val default: TestExecutor = new TestExecutor {
    override def execute[A](timeout: Duration)(f: => A): A = f
    override def shutdown(): Unit = {}
  }
}
