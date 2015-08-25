package dog

import sbt.testing.Logger
import scalaz.{-\/, \/-}

abstract class DogListener {

  def onStart[A](obj: Dog, name: String, test: TestCase[A], logger: Logger): Unit = {}

  def onFinish[A](obj: Dog, name: String, test: TestCase[A], result: TestResult[A], logger: Logger): Unit = {}

  def onFinishAll(obj: Dog, result: List[(String, DogEvent[Any])], logger: Logger): Unit = {}
}

object DogListener {

  val empty: DogListener = new DogListener {}

  val default: DogListener = new Default

  class Default extends DogListener {

    private[this] def resultToString[A](name: String, result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results.list match {
        case List(\/-(_)) => ()
        case List(-\/(Skipped(reason))) => {
          logger.info(Console.BLUE + s"Skipped: ${name}")
          logger.info(reason + Console.RESET)
        }
        case _ => {
          logger.error(Console.RED + s"NotPassed: ${name}")
          AssertionResult.onlyNotPassed(results).zipWithIndex.foreach {
            case (c, i) => logger.error(s"${i}. ${c.toString}")
          }
          logger.error(Console.RESET)
        }
      }
      case Error(es, cs) => {
        logger.error(Console.RED + s"Error: ${name}")
        es.foreach(e => {
          e.printStackTrace()
          logger.trace(e)
        })
        cs.zipWithIndex.foreach {
          case (c, i) => {
            logger.error(s"${i}. ${c.toString}")
          }
        }
        logger.error(Console.RESET)
      }
    }

    override def onFinishAll(obj: Dog, results: List[(String, DogEvent[Any])], logger: Logger) =
      results.foreach { case (name, e) => resultToString(name, e.result, logger) }

    override def onFinish[A](obj: Dog, name: String, test: TestCase[A], result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results.list match {
        case List(\/-(_)) => logger.info(".")
        case List(-\/(Skipped(reason))) => logger.info("S")
        case x => logger.info("x")
      }
      case Error(es, cs) => logger.info("E")
    }
  }
}
