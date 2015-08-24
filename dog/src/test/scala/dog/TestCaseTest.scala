package dog

object TestCaseTest extends Dog {

  val returnValue: TestCase[Unit] = {
    val target = for {
      a <- TestCase.ok(0)
    } yield a
    Assert.equal(TestResult(0), target.run(()))
  }

  val convertTestCase: TestCase[Unit] = {
    val target = for {
      a <- Assert.pass(())
    } yield a
    Assert.equal(TestResult(()), target.run(()))
  }

  val runMultpleAssertion: TestCase[Unit] = {
    val assert0 = Assert.pass(())
    val assert1 = Assert.pass(())
    val assert2 = Assert.pass(())
    val target = for {
      a <- assert0 +> assert1 +> assert2
    } yield a
    Assert.equal(TestResult(()), target.run(()))
  }

  val recordMultipleViolations: TestCase[Unit] = {
    val target = for {
      a <-
        Assert.equal(1, 2) +>
        Assert.equal(2, 3)
    } yield a
    val expected = TestResult.nel(NotPassed[Unit](Violated("expected: 1, but was: 2")), List(AssertionResult[Unit](Violated("expected: 2, but was: 3"))))
    Assert.equal(expected, target.run(()))
  }

  val bindValue: TestCase[Unit] = {
    val target = for {
      a <- TestCase.ok(0)
      _ <- Assert.equal(a, 0)
    } yield a
    Assert.equal(TestResult(0), target.run(()))
  }

  val skipTestCase: TestCase[Unit] = {
    val target = (for {
      a <- TestCase.ok(0)
    } yield a).skip("skip")
    val expected = TestResult.nel(NotPassed(NotPassedCause.skip("skip")), List())
    Assert.equal(expected, target.run(()))
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
}
