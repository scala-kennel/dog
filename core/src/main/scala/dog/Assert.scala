package dog

object Assert {

  def equal[A](expected: A, actual: A): AssertionResult[Unit] =
    if(expected == actual) pass(())
    // TODO: implement pretty printer
    else AssertionResult(Violated(s"expected: ${expected.toString}, but was: ${actual.toString}"))

  def eq[A](expected: A, actual: A)(implicit E: scalaz.Equal[A]): AssertionResult[Unit] =
    if(E.equal(expected, actual)) pass(())
    // TODO: implement pretty printer
    else AssertionResult(Violated(s"expected: ${expected.toString}, but was: ${actual.toString}"))

  def pass[A](value: A): AssertionResult[A] = AssertionResult.pass(value)

  def fail[A](reason: String): AssertionResult[A] = AssertionResult[A](NotPassedCause.violate(reason))

  def pred(p: Boolean): AssertionResult[Unit] =
    if(p) pass(())
    else fail("fail assertion.")
}
