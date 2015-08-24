package dog

import java.lang.reflect.Method
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import sbt.testing._
import scala.util.control.NonFatal
import scalaz._
import scala.reflect.NameTransformer

// port from https://github.com/scalaprops/scalaprops/blob/e6ffd8bc3d7d556e98f1ff59600d4a5c9e8dbded/scalaprops/src/main/scala/scalaprops/ScalapropsRunner.scala
// license: MIT
// author: Kenji Yoshida <https://github.com/xuwei-k>
object DogRunner {

  private[this] def findTestFields(clazz: Class[_], fieldType: Class[_]): Array[Method] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 && method.getReturnType == fieldType
    )

  private def invokeTest(clazz: Class[_], obj: Dog): List[(String, TestCase[Any])] =
    findTestFields(clazz, classOf[TestCase[_]]).map{ method =>
      val p = method.invoke(obj).asInstanceOf[TestCase[Any]]
      NameTransformer.decode(method.getName) -> p
    }.toList

  private def logger(loggers: Array[Logger]): Logger = new Logger {
    override def warn(msg: String): Unit =
      loggers.foreach(_.warn(msg))
    override def error(msg: String): Unit =
      loggers.foreach(_.error(msg))
    override def ansiCodesSupported(): Boolean =
      loggers.forall(_.ansiCodesSupported())
    override def debug(msg: String): Unit =
      loggers.foreach(_.debug(msg))
    override def trace(t: Throwable): Unit =
      loggers.foreach(_.trace(t))
    override def info(msg: String): Unit =
      loggers.foreach(_.info(msg))
  }
}

final class DogRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

  private[this] val successCount = new AtomicInteger
  private[this] val failureCount = new AtomicInteger
  private[this] val errorCount = new AtomicInteger
  private[this] val ignoredCount = new AtomicInteger
  private[this] val testCount = new AtomicInteger

  private[this] val taskdef2task: TaskDef => Task = { taskdef =>

    val testClassName = taskdef.fullyQualifiedName()
    val emptyThrowable = new OptionalThrowable

    new Task {

      override def taskDef() = taskdef

      override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {

        val log = DogRunner.logger(loggers)

        val clazz = testClassLoader.loadClass(testClassName + "$")
        val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Dog]
        val tests = DogRunner.invokeTest(clazz, obj)
        tests.foreach { case (name, test) =>
          val selector = new TestSelector(name)
          def event(status: Status, duration: Long, result0: Throwable \/ TestResult[Any]): Event = {
            val err = result0 match {
              case -\/(e) => new OptionalThrowable(e)
              case _ => emptyThrowable
            }
            DogEvent(testClassName, taskdef.fingerprint(), selector, status, err, duration, result0)
          }

          val start = System.currentTimeMillis()
          val r = try {
            // TODO: cancel
            val r = test.run(())
            val duration = System.currentTimeMillis() - start
            r match {
              case Done(results) => results.list match {
                case List(Passed(_)) =>
                  successCount.incrementAndGet()
                  event(Status.Success, duration, \/-(r))
                case List(NotPassed(Skipped(_))) =>
                  failureCount.incrementAndGet()
                  event(Status.Ignored, duration, \/-(r))
                case _ =>
                  errorCount.incrementAndGet()
                  event(Status.Failure, duration, \/-(r))
              }
              case Error(_, _) => event(Status.Error, duration, \/-(r))
            }
          } catch {
            case NonFatal(e) =>
              val duration = System.currentTimeMillis() - start
              log.trace(e)
              errorCount.incrementAndGet()
              event(Status.Error, duration, -\/(e))
          } finally {
            testCount.incrementAndGet()
          }
          eventHandler.handle(r)
        }
        Array()
      }

      override def tags() = Array()
    }
  }

  override def tasks(taskDefs: Array[TaskDef]) = taskDefs.map(taskdef2task)

  override def done() = Seq(
    s"Total test count: $testCount",
    s"Failed $failureCount, Errors $errorCount, Passed $successCount, Ignored $ignoredCount"
  ).map(Console.CYAN + _).mkString(sys.props("line.separator"))
}
