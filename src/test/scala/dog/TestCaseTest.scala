package dog

import org.scalatest.FlatSpec
import org.scalatest.DiagrammedAssertions

class TestCaseTest extends FlatSpec with DiagrammedAssertions {

  "TestCase" should "return value" in {
    val target = TestCase(for {
      a <- TestResult(0)
    }yield a)
    assert(target.run(()) == TestResult(0))
  }

  "Assertion" should "run multple assertion" in {
    val assert0 = AssertionResult.pass(())
    val assert1 = AssertionResult.pass(())
    val assert2 = AssertionResult.pass(())
    val target = TestCase[Unit](for {
      a <- Assertions(
        (assert0 |+| assert1).<::(assert2)
      )
    }yield a)
    assert(target.run(()) == TestResult(()))
  }

  "Assertion" should "record multiple violations" in {
    val target = TestCase[Unit](for {
      a <- Assertions(
        Assert.equal(1, 2) |+|
        Assert.equal(2, 3)
      )
    }yield a)
    val expected = TestResult.nel(NotPassed[Unit](Violated("expected: 1, but was: 2")), List(AssertionResult[Unit](Violated("expected: 2, but was: 3"))))
    assert(target.run(()) == expected)
  }

  "TestCase" should "bind value" in {
    val target = TestCase(for {
      a <- TestResult(0)
      _ <- Assertions(Assert.equal(a, 0))
    }yield a)
    assert(target.run(()) == TestResult(0))
  }
}
