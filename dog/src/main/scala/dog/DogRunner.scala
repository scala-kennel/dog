package dog

import java.lang.reflect.Method
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

  def allTests(clazz: Class[_], obj: Dog, only: Option[NonEmptyList[String]], logger: Logger): List[(String, TestCase[Any])] = {
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

  private[dog] def logger(loggers: Array[Logger]): Logger = new Logger {
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

  private[dog] def taskdef2task(args: Array[String], remoteArgs: Array[String], loader: ClassLoader, tracer: DogTracer): TaskDef => Task = { taskdef =>
    val testClassName = taskdef.fullyQualifiedName()
    new DogTask(args, taskdef, testClassName, loader, tracer)
  }
}

final class DogRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader,
  taskdef2task: (Array[String], Array[String], ClassLoader, DogTracer) => (TaskDef => Task)
) extends Runner {

  val tracer = new DogTracer()

  override def tasks(taskDefs: Array[TaskDef]) =
    taskDefs.map(taskdef2task(args, remoteArgs, testClassLoader, tracer))

  override def done() = tracer.done
}
