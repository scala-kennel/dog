package dog

import org.scalatest.FlatSpec
import org.scalatest.DiagrammedAssertions

class TestCaseTest extends FlatSpec with DiagrammedAssertions {

  "TestCase" should "return value" in {
    val target = for {
      a <- TestCase.ok(0)
    } yield a
    assert(target.run(()) == TestResult(0))
  }

  "Assertion" should "convert TestCase" in {
    val target = for {
      a <- Assert.pass(())
    } yield a
    assert(target.run(()) == TestResult(()))
  }

  "Assertion" should "run multple assertion" in {
    val assert0 = Assert.pass(())
    val assert1 = Assert.pass(())
    val assert2 = Assert.pass(())
    val target = for {
      a <- assert0 +> assert1 +> assert2
    } yield a
    assert(target.run(()) == TestResult(()))
  }

  "Assertion" should "record multiple violations" in {
    val target = for {
      a <-
        Assert.equal(1, 2) +>
        Assert.equal(2, 3)
    } yield a
    val expected = TestResult.nel(NotPassed[Unit](Violated("expected: 1, but was: 2")), List(AssertionResult[Unit](Violated("expected: 2, but was: 3"))))
    assert(target.run(()) == expected)
  }

  "TestCase" should "bind value" in {
    val target = for {
      a <- TestCase.ok(0)
      _ <- Assert.equal(a, 0)
    } yield a
    assert(target.run(()) == TestResult(0))
  }

  "TestCase" should "skip" in {
    val target = (for {
      a <- TestCase.ok(0)
    } yield a).skip("skip")
    assert(target.run(()) == TestResult.nel(NotPassed(NotPassedCause.skip("skip")), List()))
  }

}
