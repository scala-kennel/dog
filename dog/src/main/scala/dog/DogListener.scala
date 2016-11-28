package dog

import sbt.testing.Logger
import scalaz._

abstract class DogListener {

  def onBeforeAll(obj: Dog, tests: List[String], logger: Logger): Unit = {}

  def onStart[A](obj: Dog, name: String, logger: Logger): Unit = {}

  def onFinish[A](obj: Dog, name: String, result: TestResult[A], logger: Logger): Unit = {}

  def onFinishAll(obj: Dog, result: List[(String, DogEvent[Any])], logger: Logger): Unit = {}
}

object DogListener {

  val empty: DogListener = new DogListener {}

  val default: DogListener = new Default

  class Default extends DogListener {

    import TestResult._

    private[this] def colored(color: String): String => String = _.split("\n").map(color + _ + Console.RESET).mkString("\n")
    private[this] val blue = colored(Console.BLUE)
    private[this] val red = colored(Console.RED)

    private[this] def resultToString[A](name: String, result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results match {
        case NonEmptyList(\/-(_), INil()) => ()
        case NonEmptyList(-\/(Skipped(reason)), INil()) =>
          logger.info(blue(s"Skipped: $name"))
          logger.info(blue(reason))
        case _ =>
          logger.error(red(s"NotPassed: $name"))
          AssertionResult.onlyNotPassed(results).zipWithIndex.toList.foreach {
            case (c, i) => logger.error(red(s"$i. $c"))
          }
      }
      case Error(es, cs) =>
        logger.error(red(s"Error: $name"))
        es.toList.foreach(e => {
          e.printStackTrace()
          logger.trace(e)
        })
        cs.zipWithIndex.toList.foreach {
          case (c, i) => logger.error(red(s"$i. $c"))
        }
    }

    override def onFinishAll(obj: Dog, results: List[(String, DogEvent[Any])], logger: Logger) =
      results.foreach { case (name, e) => resultToString(name, e.result, logger) }

    override def onFinish[A](obj: Dog, name: String, result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results match {
        case NonEmptyList(\/-(_), INil()) => logger.info(".")
        case NonEmptyList(-\/(Skipped(reason)), INil()) => logger.info("S")
        case _ => logger.info("x")
      }
      case Error(es, cs) => logger.info("E")
    }
  }
}
