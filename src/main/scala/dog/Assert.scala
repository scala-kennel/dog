package dog

object Assert {

  def equal[A](expected: A, actual: A): AssertionResult[Unit] =
    if(expected == actual) AssertionResult.pass(())
    // TODO: implement pretty printer
    else AssertionResult(Violated(s"expected: ${expected.toString}, but was: ${actual.toString}"))
}
