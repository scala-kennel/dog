package dog

object ParameterizeTest extends Dog with Assert {

  val returnSampleValue = Parameterize((i: Int) => {
    val target = pass(i)
    equal(TestResult(i), TestCaseTest.run(target))
  })
}
