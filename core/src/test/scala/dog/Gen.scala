package dog

import scalaprops._

object TestGen {

  implicit val notPassedCause: Gen[NotPassedCause] =
    Gen.asciiString.flatMap(s => Gen.oneOf(Gen.value(Skipped(s)), Gen.value(Violated(s))))

  implicit def testResult[A](implicit G: Gen[A], T: Gen[Throwable]): Gen[TestResult[A]] = {
    val violated = Gen.asciiString.map(NotPassedCause.violate(_))
    lazy val error: Gen[TestResult[A]] = for {
      es <- Gen.list(T)
      cs <- if (es.isEmpty) Gen.listOf(violated, 1) else Gen.ilist(violated)
    } yield Error(es, cs.toList)
    lazy val done: Gen[TestResult[A]] =
      Gen.oneOf(
        G.map(TestResult(_)),
        notPassedCause.flatMap(n =>
          Gen.list(Gen.disjunction[NotPassedCause, A])
            .map(ns => TestResult.nel(scalaz.-\/(n), ns)))
      )
    Gen.oneOf(error, done)
  }
}
