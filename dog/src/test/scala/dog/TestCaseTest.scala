package dog

import scalaz._
import scalaz.Kleisli._

object TestCaseTest extends Dog with Assert {

  def runM[A](test: TestCase[A]): TestResult[A] =
    test.foldMap(testCaseRunner)(kleisliMonadReader).run(Param.id)

  def run[A](test: TestCaseAp[A]): TestResult[A] =
    test.foldMap(testCaseApRunner)(kleisliApplicative).run(Param.id).toTestResult

  val `return value` = TestCase {
    val target = pass(0)
    equal(TestResult(0), run(target))
  }

  val `run multple Assertion` = TestCase {
    val target = Assertion3(
      pass(()),
      pass(()),
      pass(())
    )
    equal(TestResult(()), run(target))
  }

  val `record multiple violations` = TestCase {
    val target = Assertion2(
      equal(1, 2),
      equal(2, 3)
    )
    val second: IList[AssertionResult[Unit]] =
      IList.single(\/.left(Violated("expected: 2, but was: 3")))
    val expected = TestResult.nel(-\/(Violated("expected: 1, but was: 2")), second)
    equal(expected, run(target))
  }

  val `bind value` = TestCase {
    val target = for {
      a <- pass(0).monadic
      _ <- equal(a, 0).monadic
    } yield a
    equal(TestResult(0), runM(target))
  }

  val `skip TestCase` = TestCase {
    def f: Int = throw new Exception("skip test")
    val target = pass(f).skip("skip")
    val expected = TestResult.nel(-\/(NotPassedCause.skip("skip")), IList.empty[AssertionResult[Int]])
    equal(expected, run(target))
  }

  val `trap Exception` = TestCase {
    lazy val f: Int = throw new Exception("exception test")
    val target = trap(f)
    for {
      e <- target
      _ <- equal("exception test", e.getMessage).monadic
    } yield ()
  }

  val `side effect should be executed once` = TestCase {
    var value = 0
    val test0 = {
      value += 1
      pass(0)
    }
    val test1 = for {
      a <- test0.monadic
      _ <- equal(0, a).monadic
    } yield ()
    val test2 = for {
      a <- test0.monadic
      _ <- equal(0, a).monadic
    } yield ()
    val target = for {
      _ <- test1
      _ <- test2
    } yield ()
    val actual = runM(target)
    Assertion2(
      equal(TestResult(()), actual),
      equal(1, value)
    )
  }

  val `side effect should be executed twice` = TestCase {
    var value = 0
    def test0 = {
      value += 1
      pass(0)
    }
    val test1 = for {
      a <- test0.monadic
      _ <- equal(0, a).monadic
    } yield ()
    val test2 = for {
      a <- test0.monadic
      _ <- equal(0, a).monadic
    } yield ()
    val target = for {
      a <- test1
      b <- test2
    } yield ()
    val actual = runM(target)
    Assertion2(
      equal(TestResult(()), actual),
      equal(2, value)
    )
  }

  val `setup fixture` = TestCase {
    import scala.collection.mutable.ListBuffer
    val xs = ListBuffer.empty[Int]
    for {
      _ <- TestCase.fixture(() => xs += 1)
      _ <- equal(ListBuffer[Int](1), xs).monadic
    } yield ()
  }
}
