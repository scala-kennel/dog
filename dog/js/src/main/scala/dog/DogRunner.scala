package dog

import sbt.testing._

import scala.scalajs._
import scala.scalajs.js.WrappedDictionary
import scala.reflect.NameTransformer
import scalaz.{Foldable, NonEmptyList}

object DogRunner {

  private[this] def findTestFields[A](obj: js.Dictionary[A], fieldType: Class[_]): WrappedDictionary[A] =
    obj.filter{ case (k, v) =>
      fieldType.isInstance(v)
    }.map{ case (k, v) =>
      NameTransformer.decode(k).replaceAllLiterally("$1", "") -> v
    }

  private def invokeTest[A](obj: js.Dictionary[A]): List[(String, TestCase[Any])] =
    findTestFields(obj, classOf[TestCase[_]]).map{ case (k, v) =>
      k -> v.asInstanceOf[TestCase[Any]]
    }.toList

  def allTests(obj: Dog, only: Option[NonEmptyList[String]], logger: Logger): List[(String, TestCase[Any])] = {
    val tests = invokeTest(obj.asInstanceOf[js.Dictionary[_]])
    only match {
      case Some(names) =>
        val set = Foldable[NonEmptyList].toSet(names)
        val actualTests: Set[String] = tests.map(_._1)(collection.breakOut)
        set.filterNot(actualTests).foreach{ typo =>
          //logger.warn(s"""'${obj.toString.dropRight(1)}.$typo' does not exists""")
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

  private[dog] def taskdef2task(loader: ClassLoader): TaskDef => Task = { taskdef =>
    new DogTask(taskdef, loader)
  }
}

final class DogRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader,
  taskdef2task: (ClassLoader) => (TaskDef => Task)
) extends Runner {

  override def tasks(taskDefs: Array[TaskDef]) =
    taskDefs.map(taskdef2task(testClassLoader))

  override def done() = ""

  override def receiveMessage(msg: String) = None

  override def serializeTask(task: Task, serializer: TaskDef => String) =
    serializer(task.taskDef())

  override def deserializeTask(task: String, deserializer: String => TaskDef) =
    taskdef2task(testClassLoader)(deserializer(task))
}
