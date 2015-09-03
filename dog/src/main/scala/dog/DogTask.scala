package dog

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.ForkJoinPool
import sbt.testing._
import scalaz._

private[dog] class DogTask(
  args: Array[String],
  val taskdef: TaskDef,
  val testClassName: String,
  testClassLoader: ClassLoader,
  tracer: DogTracer) extends Task {

  override def taskDef() = taskdef

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {

    val log = DogRunner.logger(loggers)

    lazy val executorService: ForkJoinPool = DogTask.createForkJoinPool(log)

    val only = scalaz.std.list.toNel(
      args.dropWhile("--only" != _).drop(1).takeWhile(arg => !arg.startsWith("--")).toList
    )

    try {
      val clazz = testClassLoader.loadClass(testClassName + "$")
      val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Dog]
      val tests = DogRunner.allTests(clazz, obj, only, log)
      val results = tests.map { case (name, test) =>
        val selector = new TestSelector(name)
        def event(status: Status, duration: Long, result: TestResult[Any]): DogEvent[Any] =
          DogTask.event(this, status, selector, duration, result)

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
                tracer.success()
                event(Status.Success, duration, r)
              case List(-\/(Skipped(_))) =>
                tracer.ignore()
                event(Status.Ignored, duration, r)
              case _ =>
                tracer.failure()
                event(Status.Failure, duration, r)
            }
            case Error(_, _) =>
              tracer.error()
              event(Status.Error, duration, r)
          }
        } finally {
          tracer.total()
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

private[dog] object DogTask {

  def createForkJoinPool(log: Logger): ForkJoinPool = {
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
    executorService
  }

  private[this] val emptyThrowable = new OptionalThrowable

  def event(task: DogTask, status: Status, selector: Selector, duration: Long, result: TestResult[Any]): DogEvent[Any] = {
    val err = result.hasError match {
      case Some(e) => new OptionalThrowable(e)
      case None => emptyThrowable
    }
    DogEvent(task.testClassName, task.taskdef.fingerprint(), selector, status, err, duration, result)
  }
}
