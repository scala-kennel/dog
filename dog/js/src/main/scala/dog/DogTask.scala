package dog

import org.scalajs.testinterface.TestUtils
import sbt.testing._

import scalaz._

private[dog] final class DogTask(
  override val taskDef: TaskDef,
  testClassLoader: ClassLoader) extends Task {

  override def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {

    val log = DogRunner.logger(loggers)

    val testClassName = taskDef.fullyQualifiedName()
    val obj = taskDef.fingerprint() match {
      case fingerprint: SubclassFingerprint if fingerprint.superclassName() == "dog.Dog" =>
        if (fingerprint.isModule) {
          TestUtils.loadModule(testClassName, testClassLoader) match {
            case m: Dog => m
            case x => throw new Exception(s"Cannot test $taskDef of type: $x")
          }
        }
        else {
          throw new Exception("FunSuite only works on objects, classes don't work.")
        }
      case _ => throw new Exception("can not find scalaporps.Scalaprops instance.")
    }
    val tests = DogRunner.allTests(obj, None, log)
    obj.listener.onBeforeAll(obj, tests, log)
    val results = tests.map { case (name, test) =>
      val selector = new TestSelector(name)
      def event(status: Status, duration: Long, result: TestResult[Any]): DogEvent[Any] =
        DogTask.event(testClassName, this, status, selector, duration, result)

      val param = obj.paramEndo
      val start = System.currentTimeMillis()
      val r = try {
        obj.listener.onStart(obj, name, test, log)
        val r = test.run(param)
        val duration = System.currentTimeMillis() - start
        obj.listener.onFinish(obj, name, test, r, log)
        r match {
          case Done(results) => results match {
            case NonEmptyList(\/-(_), INil()) =>
              event(Status.Success, duration, r)
            case NonEmptyList(-\/(Skipped(_)), INil()) =>
              event(Status.Ignored, duration, r)
            case _ =>
              event(Status.Failure, duration, r)
          }
          case Error(_, _) =>
            event(Status.Error, duration, r)
        }
      } finally {
      }
      eventHandler.handle(r)
      (name, r)
    }
    obj.listener.onFinishAll(obj, results.toList, log)
    Array()
  }

  override def tags() = Array()
}

private[dog] object DogTask {

  private[this] val emptyThrowable = new OptionalThrowable

  def event(testClassName: String, task: DogTask, status: Status, selector: Selector, duration: Long, result: TestResult[Any]): DogEvent[Any] = {
    val err = result.hasError match {
      case Some(e) => new OptionalThrowable(e)
      case None => emptyThrowable
    }
    DogEvent(testClassName, task.taskDef.fingerprint(), selector, status, err, duration, result)
  }
}
