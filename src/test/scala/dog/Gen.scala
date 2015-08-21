package dog

import scalaz._
import scalaprops._

object TestGen {

  implicit val notPassedCause: Gen[NotPassedCause] =
    Gen.asciiString.flatMap(s => Gen.oneOf(Gen.value(Skipped(s)), Gen.value(Violated(s))))

  def passed[A](implicit G: Gen[A]): Gen[AssertionResult[A]] =
    G.map(Passed(_))

  def notPassed[A]: Gen[AssertionResult[A]] = notPassedCause.map(NotPassed(_))

  implicit def assertionResult[A: Gen]: Gen[AssertionResult[A]] = {
    lazy val p: Gen[AssertionResult[A]] = passed
    lazy val n: Gen[AssertionResult[A]] = notPassed
    Gen.oneOf(p, n)
  }

  implicit def testResult[A: Gen](implicit T: Gen[Throwable]): Gen[TestResult[A]] = {
    lazy val error: Gen[TestResult[A]] = for {
      es <- Gen.list(T)
      cs <- if (es.isEmpty) Gen.listOf(notPassedCause, 1) else Gen.ilist(notPassedCause)
    } yield Error(es, cs.toList)
    lazy val done: Gen[TestResult[A]] =
      Gen.oneOf(
        passed[A].map(p => Done(NonEmptyList.nel(p, List()))),
        Gen.nonEmptyList(notPassed[A]).map(Done(_))
      )
    Gen.oneOf(error, done)
  }
}
