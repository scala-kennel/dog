package dog

import scalaprops._

object TestGen {

  implicit val notPassedCause: Gen[NotPassedCause] =
    Gen.asciiString.flatMap(s => Gen.oneOf(Gen.value(Skipped(s)), Gen.value(Violated(s))))

  implicit def testResult[A](implicit G: Gen[A], T: Gen[Throwable]): Gen[TestResult[A]] = {
    val violated = Gen.asciiString.map(NotPassedCause.violate(_))
    lazy val error: Gen[TestResult[A]] = for {
      es <- Gen.ilist(T)
      cs <- if (es.isEmpty) Gen.listOf(violated, 1) else Gen.ilist(violated)
    } yield TestResult.Error(es, cs)
    lazy val done: Gen[TestResult[A]] =
      Gen.oneOf(
        G.map(TestResult(_)),
        notPassedCause.flatMap(n =>
          Gen.ilist(Gen.disjunction[NotPassedCause, A])
            .map(ns => TestResult.nel(scalaz.-\/(n), ns)))
      )
    Gen.oneOf(error, done)
  }

  implicit def validationResult[A](implicit G: Gen[A], T: Gen[Throwable]): Gen[ValidationResult[A]] = {
    val violated = Gen.asciiString.map(NotPassedCause.violate(_))
    lazy val error: Gen[ValidationResult[A]] = for {
      es <- Gen.ilist(T)
      cs <- if (es.isEmpty) Gen.listOf(violated, 1) else Gen.ilist(violated)
    } yield ValidationResult.Error(es, cs)
    lazy val done: Gen[ValidationResult[A]] =
      Gen.oneOf(
        G.map(ValidationResult(_)),
        notPassedCause.flatMap(n =>
          Gen.ilist(Gen.disjunction[NotPassedCause, A])
            .map(ns => ValidationResult.nel(scalaz.-\/(n), ns)))
      )
    Gen.oneOf(error, done)
  }
}
