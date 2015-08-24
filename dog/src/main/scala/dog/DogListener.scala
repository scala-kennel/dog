package dog

import sbt.testing.Logger
import scalaz._

abstract class DogListener {

  def onStart[A](obj: Dog, name: String, test: TestCase[A], logger: Logger): Unit = {}

  def onFinish[A](obj: Dog, name: String, test: TestCase[A], result: TestResult[A], logger: Logger): Unit = {}

  def onFinishAll(obj: Dog, result: List[(String, DogEvent[Any])], logger: Logger): Unit = {}

  def onError(obj: Dog, name: String, e: Throwable, logger: Logger): Unit = {}
}

object DogListener {

  val empty: DogListener = new DogListener {}

  val default: DogListener = new Default

  class Default extends DogListener {

    private[this] def resultToString[A](name: String, result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results.list match {
        case List(Passed(_)) => ()
        case List(NotPassed(Skipped(reason))) => {
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

    override def onFinishAll(obj: Dog, results: List[(String, DogEvent[Any])], logger: Logger) = results.foreach { case (name, e) =>
      e.result match {
        case -\/(e) => {
          logger.error(Console.RED + s"Error: ${name}")
          e.printStackTrace()
          logger.trace(e)
          logger.error(Console.RESET)
        }
        case \/-(r) => resultToString(name, r, logger)
      }
    }

    override def onFinish[A](obj: Dog, name: String, test: TestCase[A], result: TestResult[A], logger: Logger) = result match {
      case Done(results) => results.list match {
        case List(Passed(_)) => logger.info(".")
        case List(NotPassed(Skipped(reason))) => logger.info("S")
        case x => logger.info("x")
      }
      case Error(es, cs) => logger.info("E")
    }

    override def onError(obj: Dog, name: String, e: Throwable, logger: Logger): Unit = {
      logger.error(s"error ${obj.getClass.getCanonicalName} $name")
      logger.trace(e)
      e.printStackTrace()
    }
  }
}
