package dog

import java.lang.Thread.UncaughtExceptionHandler
import java.lang.reflect.Method
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicInteger
import sbt.testing._
import scala.reflect.NameTransformer
import scalaz._

// port from https://github.com/scalaprops/scalaprops/blob/e6ffd8bc3d7d556e98f1ff59600d4a5c9e8dbded/scalaprops/src/main/scala/scalaprops/ScalapropsRunner.scala
// license: MIT
// author: Kenji Yoshida <https://github.com/xuwei-k>
object DogRunner {

  def testFieldNames(clazz: Class[_]): Array[String] =
    findTestFields(clazz, classOf[TestCase[_]]).map(_.getName)

  private[this] def findTestFields(clazz: Class[_], fieldType: Class[_]): Array[Method] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 && method.getReturnType == fieldType
    )

  private def invokeTest(clazz: Class[_], obj: Dog): List[(String, TestCase[Any])] =
    findTestFields(clazz, classOf[TestCase[_]]).map{ method =>
      val p = method.invoke(obj).asInstanceOf[TestCase[Any]]
      NameTransformer.decode(method.getName) -> p
    }.toList

  private def allTests(clazz: Class[_], obj: Dog, only: Option[NonEmptyList[String]], logger: Logger): List[(String, TestCase[Any])] = {
    val tests = invokeTest(clazz, obj)
    only match {
      case Some(names) =>
        val set = Foldable[NonEmptyList].toSet(names)
        val actualTests: Set[String] = tests.map(_._1)(collection.breakOut)
        set.filterNot(actualTests).foreach{ typo =>
          logger.warn(s"""'${clazz.getCanonicalName.dropRight(1)}.$typo' does not exists""")
        }
        tests.filter { case (n, _) => set(n) }
      case None =>
        tests
    }
  }

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

        lazy val executorService: ForkJoinPool = new ForkJoinPool(
          sys.runtime.availableProcessors(),
          ForkJoinPool.defaultForkJoinWorkerThreadFactory,
          new UncaughtExceptionHandler {
            def uncaughtException(t: Thread, e: Throwable): Unit = {
              log.error("uncaughtException Thread = " + t)
              log.trace(e)
              e.printStackTrace()
              executorService.shutdown()
            }
          },
          false
        )

        val only = scalaz.std.list.toNel(
          args.dropWhile("--only" != _).drop(1).takeWhile(arg => !arg.startsWith("--")).toList
        )

        try {
          val clazz = testClassLoader.loadClass(testClassName + "$")
          val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Dog]
          val tests = DogRunner.allTests(clazz, obj, only, log)
          val results = tests.map { case (name, test) =>
            val selector = new TestSelector(name)
            def event(status: Status, duration: Long, result0: TestResult[Any]): DogEvent[Any] = {
              val err = result0.hasError match {
                case Some(e) => new OptionalThrowable(e)
                case None => emptyThrowable
              }
              DogEvent(testClassName, taskdef.fingerprint(), selector, status, err, duration, result0)
            }

            val param = obj.paramEndo compose Param.executorService(executorService)
            val start = System.currentTimeMillis()
            val r = try {
              obj.listener.onStart(obj, name, test, log)
              val r = test.run(param)
              val duration = System.currentTimeMillis() - start
              obj.listener.onFinish(obj, name, test, r, log)
              r match {
                case Done(results) => results.list match {
                  case List(\/-(_)) =>
                    successCount.incrementAndGet()
                    event(Status.Success, duration, r)
                  case List(-\/(Skipped(_))) =>
                    ignoredCount.incrementAndGet()
                    event(Status.Ignored, duration, r)
                  case _ =>
                    failureCount.incrementAndGet()
                    event(Status.Failure, duration, r)
                }
                case Error(_, _) =>
                  errorCount.incrementAndGet()
                  event(Status.Error, duration, r)
              }
            } finally {
              testCount.incrementAndGet()
            }
            eventHandler.handle(r)
            (name, r)
          }
          obj.listener.onFinishAll(obj, results.toList, log)
          Array()
        } finally {
          executorService.shutdown()
        }
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
