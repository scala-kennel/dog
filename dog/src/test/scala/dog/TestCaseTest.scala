package dog

import scalaz._
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

object TestCaseTest extends Dog {

  def run[A](test: TestCase[A]) = test.run(Param.id)

  val returnValue: TestCase[Unit] = {
    val target = for {
      a <- TestCase.ok(0)
    } yield a
    Assert.equal(TestResult(0), run(target))
  }

  val convertTestCase: TestCase[Unit] = {
    val target = for {
      a <- Assert.pass(())
    } yield a
    Assert.equal(TestResult(()), run(target))
  }

  val runMultpleAssertion: TestCase[Unit] = {
    val assert0 = Assert.pass(())
    val assert1 = Assert.pass(())
    val assert2 = Assert.pass(())
    val target = for {
      a <- assert0 +> assert1 +> assert2
    } yield a
    Assert.equal(TestResult(()), run(target))
  }

  val recordMultipleViolations: TestCase[Unit] = {
    val target = for {
      a <-
        Assert.equal(1, 2) +>
        Assert.equal(2, 3)
    } yield a
    val second: IList[AssertionResult[Unit]] =
      IList.single(\/.left(Violated("expected: 2, but was: 3")))
    val expected = TestResult.nel(-\/(Violated("expected: 1, but was: 2")), second)
    Assert.equal(expected, run(target))
  }

  val bindValue: TestCase[Unit] = {
    val target = for {
      a <- TestCase.ok(0)
      _ <- Assert.equal(a, 0)
    } yield a
    Assert.equal(TestResult(0), run(target))
  }

  val skipTestCase: TestCase[Unit] = {
    val target = (for {
      a <- TestCase.ok(0)
    } yield a).skip("skip")
    val expected = TestResult.nel(-\/(NotPassedCause.skip("skip")), IList.empty[AssertionResult[Int]])
    Assert.equal(expected, run(target))
  }

  val trapException = {
    lazy val f: Int = throw new Exception("exception test")
    val target: TestCase[Throwable] = for {
      e <- Assert.trap(f)
    } yield e
    for {
      e <- target
      _ <- Assert.equal("exception test", e.getMessage)
    } yield ()
  }

  def isTimeout(target: Throwable): AssertionResult[Unit] = target match {
    case a: TimeoutException => Assert.pass(())
    case _ => Assert.fail[Unit]("expected TimeoutException, but not")
  }

  val handleTimeout: TestCase[Unit] = {
    val target = TestCase[Unit] {
      Thread.sleep(100)
      TestResult.nel(-\/(NotPassedCause.violate("should timeout but not")), IList.empty)
    }.timeout(1, TimeUnit.MILLISECONDS)
    run(target) match {
      case Error(ICons(e, _), _) => isTimeout(e)
      case _ => Assert.fail[Unit]("should timeout but not")
    }
  }

  val `side effect should be executed once` = {
    var value = 0
    val test0: TestCase[Int] = {
      value += 1
      Assert.pass(0)
    }
    val test1 = for {
      a <- test0
      _ <- Assert.equal(0, a)
    } yield ()
    val test2 = for {
      a <- test0
      _ <- Assert.equal(0, a)
    } yield ()
    val target = for {
      a <- test1
      b <- test2
    } yield ()
    val actual = run(target)
    Assert.equal(TestResult(()), actual) +>
    Assert.equal(1, value)
  }

  val `side effect should be executed twice` = {
    var value = 0
    def test0 = TestCase.delay {
      value += 1
      Assert.pass(0)
    }
    val test1 = for {
      a <- test0
      _ <- Assert.equal(0, a)
    } yield ()
    val test2 = for {
      a <- test0
      _ <- Assert.equal(0, a)
    } yield ()
    val target = for {
      a <- test1
      b <- test2
    } yield ()
    val actual = run(target)
    Assert.equal(TestResult(()), actual) +>
    Assert.equal(2, value)
  }

  val testSetUpFixture = {
    var value = 0
    for {
      _ <- TestCase.fixture(() => value += 1)
      _ <- Assert.equal(1, value)
    } yield ()
  }
}
