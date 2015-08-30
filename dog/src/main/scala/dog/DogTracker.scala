package dog

import java.util.concurrent.atomic.AtomicInteger

private[dog] class DogTracer() {

  private[this] val successCount = new AtomicInteger
  private[this] val failureCount = new AtomicInteger
  private[this] val errorCount = new AtomicInteger
  private[this] val ignoredCount = new AtomicInteger
  private[this] val testCount = new AtomicInteger

  def success(): Int = successCount.incrementAndGet()
  def failure(): Int = failureCount.incrementAndGet()
  def error(): Int = errorCount.incrementAndGet()
  def ignore(): Int = ignoredCount.incrementAndGet()
  def total(): Int = testCount.incrementAndGet()

  def done: String = Seq(
    s"Total test count: $testCount",
    s"Failed $failureCount, Errors $errorCount, Passed $successCount, Ignored $ignoredCount"
  ).map(Console.CYAN + _).mkString(sys.props("line.separator"))
}
