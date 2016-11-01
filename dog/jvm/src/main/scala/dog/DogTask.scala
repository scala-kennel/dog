package dog

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.ForkJoinPool
import sbt.testing._
import scalaz._
import scalaz.Kleisli._

private[dog] class DogTask(
  args: Array[String],
  val taskdef: TaskDef,
  val testClassName: String,
  testClassLoader: ClassLoader,
  tracer: DogTracer) extends Task {

  override def taskDef() = taskdef

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {

    val log = DogRunner.logger(loggers)

    val only = scalaz.std.list.toNel(
      args.dropWhile("--only" != _).drop(1).takeWhile(arg => !arg.startsWith("--")).toList
    )

    val clazz = testClassLoader.loadClass(testClassName + "$")
    val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Dog]
    val tests = DogRunner.allTests(clazz, obj, only, log)
    obj.listener.onBeforeAll(obj, tests.map(_._1), log)
    val results = tests.map { case (name, test) =>
      val selector = new TestSelector(name)
      def event(status: Status, duration: Long, result: TestResult[Any]): DogEvent[Any] =
        DogTask.event(this, status, selector, duration, result)

      val executor = DogTask.executor(log)
      val param = obj.paramEndo compose Param.executor(executor)
      val start = System.currentTimeMillis()
      val r = try {
        obj.listener.onStart(obj, name, log)
        val r = test.fold(
          _.foldMap(obj.testCaseApRunner).run(param).toTestResult,
          _.foldMap(obj.testCaseRunner).run(param))
        val duration = System.currentTimeMillis() - start
        obj.listener.onFinish(obj, name, r, log)
        r match {
          case TestResult.Done(results) => results match {
            case NonEmptyList(\/-(_), INil()) =>
              tracer.success()
              event(Status.Success, duration, r)
            case NonEmptyList(-\/(Skipped(_)), INil()) =>
              tracer.ignore()
              event(Status.Ignored, duration, r)
            case _ =>
              tracer.failure()
              event(Status.Failure, duration, r)
          }
          case TestResult.Error(_, _) =>
            tracer.error()
            event(Status.Error, duration, r)
        }
      } finally {
        tracer.total()
        executor.shutdown()
      }
      eventHandler.handle(r)
      (name, r)
    }
    obj.listener.onFinishAll(obj, results, log)
    Array()
  }

  override def tags() = Array()
}

private[dog] object DogTask {

  def executor(log: Logger): TestExecutor = new TestExecutor {
    private[this] val executionContext = {
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
      ExecutionContext.fromExecutorService(executorService)
    }

    override def execute[A](timeout: Duration)(f: => A): A =
      Await.result(Future(f)(executionContext), timeout)

    override def shutdown(): Unit =
      executionContext.shutdown()
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
